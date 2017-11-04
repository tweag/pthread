-- | Bindings to the POSIX threads library.

{-# LANGUAGE CApiFFI #-}
{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

-- see comment on the imports of Data.Int and Data.Word
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Foreign.Concurrent.PThread
  ( -- * thread local storage
    Key
  , keyCreate
  , keyCreate_
  , keyDelete
  , setSpecific
  , getSpecific
  ) where

import Control.Concurrent (isCurrentThreadBound, rtsSupportsBoundThreads)
import Control.Monad ((>=>), unless, when)
import Foreign.C.Types
import Foreign.C.Error (Errno(..), errnoToIOError)
import Foreign.Marshal.Alloc (alloca)
import Foreign.Ptr
import Foreign.Storable
import GHC.Stack (HasCallStack, callStack, getCallStack)

-- These two might be used depending on what the Key representation expands to.
import Data.Int
import Data.Word

#include <pthread.h>

-- | Opaque objects used to locate thread-specific data.
newtype Key = Key #{type pthread_key_t}
  deriving (Eq, Ord, Show, Storable)
-- We check in cbits/checks.c that the size of pthread_key_t fits unsigned int.

foreign import capi unsafe "pthread.h"
   pthread_key_create :: Ptr Key -> FunPtr (Ptr a -> IO ()) -> IO CInt

-- | Thread-specific data key creation.
keyCreate
  :: FunPtr (Ptr a -> IO ()) -- ^ Finalizer
  -> IO Key
keyCreate destructor = alloca $ \keyPtr -> do
    throwIfNonZero_ $ pthread_key_create keyPtr destructor
    peek keyPtr

-- | Like 'keyCreate', but with no finalizer.
keyCreate_ :: IO Key
keyCreate_ = keyCreate nullFunPtr

foreign import capi unsafe "pthread.h"
   pthread_key_delete :: Key -> IO CInt

-- | Thread-specific data key deletion.
keyDelete :: Key -> IO ()
keyDelete k = throwIfNonZero_ $ pthread_key_delete k

foreign import capi unsafe "pthread.h"
   pthread_setspecific :: Key -> Ptr a -> IO CInt

-- | Associate a thread-specific /value/ with a /key/ obtained via a previous
-- call to 'keyCreate'.
setSpecific :: Key -> Ptr a -> IO ()
setSpecific k v = do
    checkBoundness
    throwIfNonZero_ $ pthread_setspecific k v

foreign import capi unsafe "pthread.h"
   pthread_getspecific :: Key -> IO (Ptr a)

-- | Return the value currently bound to the specified key on behalf of the
-- calling thread.
getSpecific :: Key -> IO (Ptr a)
getSpecific k = do
    checkBoundness
    pthread_getspecific k

-- | Yields an error if the calling thread is not bound.
checkBoundness :: IO ()
checkBoundness = when rtsSupportsBoundThreads $ do
    bound <- isCurrentThreadBound
    unless bound $
      fail "pthread: checkBoundness: Calling thread is not bound"

-- | Yields an error if the passed integer is not zero.
throwIfNonZero_ :: HasCallStack => IO CInt -> IO ()
throwIfNonZero_ m = m >>= \rc -> when (rc /= 0) $
    ioError (errnoToIOError name (Errno rc) Nothing Nothing)
  where
    (name, _):_ = getCallStack callStack
