{-# LANGUAGE CPP             #-}
{-# LANGUAGE MagicHash       #-}
{-# LANGUAGE RoleAnnotations #-}
{-# LANGUAGE UnboxedTuples   #-}

-- |
--  Module      : Data.PrimRef
--  Copyright   : Copyright (C) 2014 Christopher Chalmers
--  License     : BSD3
--
--  Maintainer  : Christopher Chalmers <c.chalmers@me.com>
--  Stability   : experimental
--  Portability : GHC
--
--  Mutable primitive variables. Essentially primitive vectors that
--  contain one element.
--

module Data.PrimRef
  ( -- * PrimRef
    PrimRef , IOPrimRef, STPrimRef

    -- * Functions on PrimRefs
  , newPrimRef
  , readPrimRef
  , writePrimRef
  , modifyPrimRef
  , modifyPrimRef'
  , unsafeAccessByteArray
  , plusOne
  ) where

import Control.Monad.Primitive
import Data.Primitive.ByteArray
import Data.Primitive.Types

import GHC.Exts

-- | Mutable variables Essentially a Primitive mutable vector only
--   containing a single object.
newtype PrimRef s a = PrimRef (MutableByteArray s)
-- type role PrimRef representational representational

type IOPrimRef   = PrimRef RealWorld
type STPrimRef s = PrimRef s

-- | Build a new 'PrimRef'
newPrimRef :: (PrimMonad m, Prim a) => a -> m (PrimRef (PrimState m) a)
newPrimRef p = primitive $ \s ->
  let (# s', arr# #) = newByteArray# (sizeOf# p) s
      s''            = writeByteArray# arr# 0# p s'
  in  (# s'', PrimRef (MutableByteArray arr#) #)
{-# INLINE newPrimRef #-}

-- | Read the value from a 'PrimRef'.
readPrimRef :: (PrimMonad m, Prim a) => PrimRef (PrimState m) a -> m a
readPrimRef (PrimRef (MutableByteArray arr#))
  = primitive (readByteArray# arr# 0#)
{-# INLINE readPrimRef #-}

-- | Write a value to a 'PrimRef'.
writePrimRef :: (PrimMonad m, Prim a) => PrimRef (PrimState m) a -> a -> m ()
writePrimRef (PrimRef (MutableByteArray arr#)) x
  = primitive_ (writeByteArray# arr# 0# x)
{-# INLINE writePrimRef #-}

-- | Modify a 'PrimRef'. Note this function is lazy and can overflow.
--   Use 'modifyPrimRef'' for a strict modify.
modifyPrimRef :: (PrimMonad m, Prim a) => PrimRef (PrimState m) a -> (a -> a) -> m ()
modifyPrimRef (PrimRef (MutableByteArray arr#)) f = primitive_ $ \s ->
  let (# s', a #) = readByteArray# arr# 0# s
  in  writeByteArray# arr# 0# (f a) s'
{-# INLINE modifyPrimRef #-}

-- | Strict version of 'modifyMutVar'
modifyPrimRef' :: (PrimMonad m, Prim a) => PrimRef (PrimState m) a -> (a -> a) -> m ()
modifyPrimRef' (PrimRef (MutableByteArray arr#)) f = primitive_ $ \s ->
  let (# s', a #) = readByteArray# arr# 0# s
      a'          = f a
  in  a' `seq` writeByteArray# arr# 0# a' s'
{-# INLINE modifyPrimRef' #-}

-- | Access the underlying 'MutableByteArray' found in the "primitive"
--   package.
unsafeAccessByteArray :: PrimMonad m => PrimRef (PrimState m) a -> (MutableByteArray (PrimState m) -> m b) -> m b
unsafeAccessByteArray (PrimRef mba) f = f mba
{-# INLINE unsafeAccessByteArray #-}

-- | Add one to a counter 'PrimRef'
plusOne :: PrimMonad m => PrimRef (PrimState m) Int -> m ()
plusOne (PrimRef (MutableByteArray arr#)) = primitive_ $ \s ->
  let (# s', i# #) = readIntArray# arr# 0# s
  in  writeIntArray# arr# 0# (i# +# 1#) s'

