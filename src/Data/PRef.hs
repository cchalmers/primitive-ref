{-# LANGUAGE CPP             #-}
{-# LANGUAGE MagicHash       #-}
{-# LANGUAGE RoleAnnotations #-}
{-# LANGUAGE UnboxedTuples   #-}

-- |
--  Module      : Data.PRef
--  Copyright   : Copyright (C) 2015 Christopher Chalmers
--  License     : BSD3
--
--  Maintainer  : Christopher Chalmers <c.chalmers@me.com>
--  Stability   : experimental
--  Portability : GHC
--
--  Mutable primitive variables. Essentially primitive mutable vectors
--  that contain one element.


module Data.PRef
  ( -- * PRef
    PRef , IOPRef

    -- * Functions on PRefs
  , newPRef
  , readPRef
  , writePRef
  , modifyPRef
  , modifyPRef'
  , unsafeAccessByteArray
  , plusOne
  ) where

import Control.Monad.Primitive
import Data.Primitive.ByteArray
import Data.Primitive.Types

import GHC.Exts

-- | Mutable variables Essentially a Primitive mutable vector only
--   containing a single object.
newtype PRef s a = PRef (MutableByteArray s)
-- type role PRef representational representational

type IOPRef   = PRef RealWorld

-- | Build a new 'PRef'
newPRef :: (PrimMonad m, Prim a) => a -> m (PRef (PrimState m) a)
newPRef p = primitive $ \s ->
  let (# s', arr# #) = newByteArray# (sizeOf# p) s
      s''            = writeByteArray# arr# 0# p s'
  in  (# s'', PRef (MutableByteArray arr#) #)
{-# INLINE newPRef #-}

-- | Read the value from a 'PRef'.
readPRef :: (PrimMonad m, Prim a) => PRef (PrimState m) a -> m a
readPRef (PRef (MutableByteArray arr#))
  = primitive (readByteArray# arr# 0#)
{-# INLINE readPRef #-}

-- | Write a value to a 'PRef'.
writePRef :: (PrimMonad m, Prim a) => PRef (PrimState m) a -> a -> m ()
writePRef (PRef (MutableByteArray arr#)) x
  = primitive_ (writeByteArray# arr# 0# x)
{-# INLINE writePRef #-}

-- | Modify a 'PRef'. Note this function is lazy and can overflow.
--   Use 'modifyPRef'' for a strict modify.
modifyPRef :: (PrimMonad m, Prim a) => PRef (PrimState m) a -> (a -> a) -> m ()
modifyPRef (PRef (MutableByteArray arr#)) f = primitive_ $ \s ->
  let (# s', a #) = readByteArray# arr# 0# s
  in  writeByteArray# arr# 0# (f a) s'
{-# INLINE modifyPRef #-}

-- | Strict version of 'modifyMutVar'
modifyPRef' :: (PrimMonad m, Prim a) => PRef (PrimState m) a -> (a -> a) -> m ()
modifyPRef' (PRef (MutableByteArray arr#)) f = primitive_ $ \s ->
  let (# s', a #) = readByteArray# arr# 0# s
      a'          = f a
  in  a' `seq` writeByteArray# arr# 0# a' s'
{-# INLINE modifyPRef' #-}

-- | Access the underlying 'MutableByteArray' found in the "primitive"
--   package.
unsafeAccessByteArray :: PrimMonad m => PRef (PrimState m) a -> (MutableByteArray (PrimState m) -> m b) -> m b
unsafeAccessByteArray (PRef mba) f = f mba
{-# INLINE unsafeAccessByteArray #-}

-- | Add one to a counter 'PRef'
plusOne :: PrimMonad m => PRef (PrimState m) Int -> m ()
plusOne (PRef (MutableByteArray arr#)) = primitive_ $ \s ->
  let (# s', i# #) = readIntArray# arr# 0# s
  in  writeIntArray# arr# 0# (i# +# 1#) s'

