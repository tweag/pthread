-- | PThread bindings

{-# LANGUAGE CApiFFI #-}
{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
-- see comment on the imports of Data.Int and Data.Word
{-# OPTIONS_GHC -Wno-unused-imports #-}
module Foreign.Concurrent.PThread
  ( -- * thread local storage
    Key
  , keyCreate
  , keyDelete
  , setSpecific
  , getSpecific
  ) where

import Control.Concurrent (isCurrentThreadBound, rtsSupportsBoundThreads)
import Control.Monad ((>=>), unless, when)
import Foreign.C.Types
import Foreign.Marshal.Alloc (alloca)
import Foreign.Ptr
import Foreign.Storable

-- These two might be used depending on what the Key representation expands to.
import Data.Int
import Data.Word

#include <pthread.h>

-- | Stands for @pthread_key_t@.
newtype Key = Key #{type pthread_key_t}
  deriving (Eq, Ord, Show, Storable)
-- We check in cbits/checks.c that the size of pthread_key_t fits unsigned int.

foreign import capi unsafe "pthread.h"
   pthread_key_create :: Ptr Key -> FunPtr (Ptr a -> IO ()) -> IO CInt

-- | Stands for @pthread_key_create@.
keyCreate :: FunPtr (Ptr a -> IO ()) -> IO Key
keyCreate destructor = alloca $ \keyPtr ->
    pthread_key_create keyPtr destructor >>= checkReturnCode >> peek keyPtr

foreign import capi unsafe "pthread.h"
   pthread_key_delete :: Key -> IO CInt

-- | Stands for @pthread_key_delete@.
keyDelete :: Key -> IO ()
keyDelete = pthread_key_delete >=> checkReturnCode

foreign import capi unsafe "pthread.h"
   pthread_setspecific :: Key -> Ptr a -> IO CInt

-- | Stands for @pthread_setspecific@.
setSpecific :: Key -> Ptr a -> IO ()
setSpecific k v = checkBoundness >> pthread_setspecific k v >>= checkReturnCode

foreign import capi unsafe "pthread.h"
   pthread_getspecific :: Key -> IO (Ptr a)

-- | Stands for @pthread_getspecific@.
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
checkReturnCode :: CInt -> IO ()
checkReturnCode rc = when (rc /= 0) $
    fail $ "pthread: checkReturnCode: non-zero return code: " ++ show rc
