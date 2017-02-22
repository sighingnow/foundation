-- |
-- Module      : Foundation.Class.Storable
-- License     : BSD-style
-- Maintainer  : Haskell Foundation
-- Stability   : experimental
-- Portability : portable
--
-- <https://github.com/haskell-foundation/issues/111>
--
--

{-# LANGUAGE CPP #-}

module Foundation.Class.Storable
    ( Storable(..)
    , StorableFixed(..)

    , Ptr, plusPtr, castPtr
    , peekOff, pokeOff

      -- * Endianness
    , ByteSwap
      -- ** Big Endian
    , BE(..), toBE, fromBE
      -- ** Little Endian
    , LE(..), toLE, fromLE
    ) where

import GHC.Types (Double, Float)

import Foreign.Ptr (castPtr)
import qualified Foreign.Ptr
import qualified Foreign.Storable (peek, poke, sizeOf, alignment)

import Foundation.Internal.Base
import Foundation.Internal.Types
import Foundation.Internal.Proxy
import Foundation.Internal.ByteSwap
import Foundation.Primitive.Types
import Foundation.Numerical

#if !defined(ARCH_IS_LITTLE_ENDIAN) && !defined(ARCH_IS_BIG_ENDIAN)
import Foundation.System.Info (endianness, Endianness(..))
#endif
toProxy :: proxy ty -> Proxy ty
toProxy _ = Proxy

-- | Storable type of self determined size.
--
class Storable a where
    peek :: Ptr a -> IO a
    poke :: Ptr a -> a -> IO ()

-- | Extending the Storable type class to the types that can be sequenced
-- in a structure.
--
class Storable a => StorableFixed a where
    size :: proxy a -> Size Word8
    alignment :: proxy a -> Size Word8

plusPtr :: StorableFixed a => Ptr a -> Size a -> Ptr a
plusPtr ptr (Size num) = ptr `Foreign.Ptr.plusPtr` (num * (size ptr `align` alignment ptr))
  where
    align (Size sz) (Size a) = sz + (sz `mod` a)

-- | like `peek` but at a given offset.
peekOff :: StorableFixed a => Ptr a -> Offset a -> IO a
peekOff ptr off = peek (ptr `plusPtr` offsetAsSize off)

-- | like `poke` but at a given offset.
pokeOff :: StorableFixed a => Ptr a -> Offset a -> a -> IO ()
pokeOff ptr off = poke (ptr `plusPtr` offsetAsSize off)

-- | Little Endian value
newtype LE a = LE { unLE :: a }
  deriving (Show, Eq, Typeable)

-- | Big Endian value
newtype BE a = BE { unBE :: a }
  deriving (Show, Eq, Typeable)

-- | Convert a value in cpu endianess to big endian
toBE :: ByteSwap a => a -> BE a
#ifdef ARCH_IS_LITTLE_ENDIAN
toBE = BE . byteSwap
#elif ARCH_IS_BIG_ENDIAN
toBE = BE
#else
toBE = BE . (if endianness == LittleEndian then byteSwap else id)
#endif
{-# INLINE toBE #-}

-- | Convert from a big endian value to the cpu endianness
fromBE :: ByteSwap a => BE a -> a
#ifdef ARCH_IS_LITTLE_ENDIAN
fromBE (BE a) = byteSwap a
#elif ARCH_IS_BIG_ENDIAN
fromBE (BE a) = a
#else
fromBE (BE a) = if endianness == LittleEndian then byteSwap a else a
#endif
{-# INLINE fromBE #-}

-- | Convert a value in cpu endianess to little endian
toLE :: ByteSwap a => a -> LE a
#ifdef ARCH_IS_LITTLE_ENDIAN
toLE = LE
#elif ARCH_IS_BIG_ENDIAN
toLE = LE . byteSwap
#else
toLE = LE . (if endianness == LittleEndian then id else byteSwap)
#endif
{-# INLINE toLE #-}

-- | Convert from a little endian value to the cpu endianness
fromLE :: ByteSwap a => LE a -> a
#ifdef ARCH_IS_LITTLE_ENDIAN
fromLE (LE a) = a
#elif ARCH_IS_BIG_ENDIAN
fromLE (LE a) = byteSwap a
#else
fromLE (LE a) = if endianness == LittleEndian then a else byteSwap a
#endif
{-# INLINE fromLE #-}

instance Storable Char where
    peek (Ptr addr) = primAddrRead addr (Offset 0)
    poke (Ptr addr) = primAddrWrite addr (Offset 0)
instance Storable Double where
    peek (Ptr addr) = primAddrRead addr (Offset 0)
    poke (Ptr addr) = primAddrWrite addr (Offset 0)
instance Storable Float where
    peek (Ptr addr) = primAddrRead addr (Offset 0)
    poke (Ptr addr) = primAddrWrite addr (Offset 0)
instance Storable Int8 where
    peek (Ptr addr) = primAddrRead addr (Offset 0)
    poke (Ptr addr) = primAddrWrite addr (Offset 0)
instance Storable Int16 where
    peek (Ptr addr) = primAddrRead addr (Offset 0)
    poke (Ptr addr) = primAddrWrite addr (Offset 0)
instance Storable Int32 where
    peek (Ptr addr) = primAddrRead addr (Offset 0)
    poke (Ptr addr) = primAddrWrite addr (Offset 0)
instance Storable Int64 where
    peek (Ptr addr) = primAddrRead addr (Offset 0)
    poke (Ptr addr) = primAddrWrite addr (Offset 0)
instance Storable Word8 where
    peek (Ptr addr) = primAddrRead addr (Offset 0)
    poke (Ptr addr) = primAddrWrite addr (Offset 0)
instance Storable Word16 where
    peek (Ptr addr) = primAddrRead addr (Offset 0)
    poke (Ptr addr) = primAddrWrite addr (Offset 0)
instance Storable Word32 where
    peek (Ptr addr) = primAddrRead addr (Offset 0)
    poke (Ptr addr) = primAddrWrite addr (Offset 0)
instance Storable Word64 where
    peek (Ptr addr) = primAddrRead addr (Offset 0)
    poke (Ptr addr) = primAddrWrite addr (Offset 0)
instance Storable (Ptr a) where
    peek = Foreign.Storable.peek
    poke = Foreign.Storable.poke
instance Storable a => Storable (BE a) where
    peek ptr = BE <$> peek (castPtr ptr)
    poke ptr (BE v) = poke (castPtr ptr) v
instance Storable a => Storable (LE a) where
    peek ptr = LE <$> peek (castPtr ptr)
    poke ptr (LE v) = poke (castPtr ptr) v

instance StorableFixed Char where
    size      = primSizeInBytes . toProxy
    alignment = primSizeInBytes . toProxy
instance StorableFixed Double where
    size      = primSizeInBytes . toProxy
    alignment = primSizeInBytes . toProxy
instance StorableFixed Float where
    size      = primSizeInBytes . toProxy
    alignment = primSizeInBytes . toProxy
instance StorableFixed Int8 where
    size      = primSizeInBytes . toProxy
    alignment = primSizeInBytes . toProxy
instance StorableFixed Int16 where
    size      = primSizeInBytes . toProxy
    alignment = primSizeInBytes . toProxy
instance StorableFixed Int32 where
    size      = primSizeInBytes . toProxy
    alignment = primSizeInBytes . toProxy
instance StorableFixed Int64 where
    size      = primSizeInBytes . toProxy
    alignment = primSizeInBytes . toProxy
instance StorableFixed Word8 where
    size      = primSizeInBytes . toProxy
    alignment = primSizeInBytes . toProxy
instance StorableFixed Word16 where
    size      = primSizeInBytes . toProxy
    alignment = primSizeInBytes . toProxy
instance StorableFixed Word32 where
    size      = primSizeInBytes . toProxy
    alignment = primSizeInBytes . toProxy
instance StorableFixed Word64 where
    size      = primSizeInBytes . toProxy
    alignment = primSizeInBytes . toProxy
instance StorableFixed (Ptr a) where
    size      = Size . Foreign.Storable.sizeOf    . toUndefined
    alignment = Size . Foreign.Storable.alignment . toUndefined
instance StorableFixed a => StorableFixed (BE a) where
    size      = size . toUndefined
    alignment = size . toUndefined
instance StorableFixed a => StorableFixed (LE a) where
    size      = size . toUndefined
    alignment = size . toUndefined

toUndefined :: proxy a -> a
toUndefined _ = undefined
