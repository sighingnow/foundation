{-# LANGUAGE UnboxedTuples #-}
module Foundation.Hashing.Hasher
    ( Hasher(..)
    ) where

import           Foundation.Internal.Base
import           Foundation.Array (UArray)
import qualified Foundation.Array.Unboxed as A
import           Data.Bits
import qualified Prelude

-- | Incremental Hashing state. Represent an hashing algorithm
--
-- the base primitive of this class is `hashMix8`, append
-- mix a Word8 in the state
--
-- The class allow to define faster mixing function that works on
-- bigger Word size and any unboxed array of any PrimType elements
class Hasher st where
    {-# MINIMAL hashNew, hashMix8, hashEnd #-}

    -- | Associate type when finalizing the state with 'hashEnd'
    type HashResult st

    -- | Create a new Hashing context
    hashNew :: st

    -- | Finalize the state and returns the hash result
    hashEnd :: st -> HashResult st

    -- | Mix a Word8 (Byte) into the state and return the new state
    hashMix8  :: Word8  -> st -> st

    -- | Mix a Word16 into the state and return the new state
    hashMix16 :: Word16 -> st -> st
    hashMix16 w st = hashMix8 w2 $ hashMix8 w1 st
      where (# !w1, !w2 #) = unWord16 w

    -- | Mix a Word32 into the state and return the new state
    hashMix32 :: Word32 -> st -> st
    hashMix32 w st = hashMix8 w4 $ hashMix8 w3 $ hashMix8 w2 $ hashMix8 w1 st
      where (# !w1, !w2, !w3, !w4 #) = unWord32 w

    -- | Mix a Word64 into the state and return the new state
    hashMix64 :: Word64 -> st -> st
    hashMix64 w st = hashMix32 w2 $ hashMix32 w1 st
      where (# !w1, !w2 #) = unWord64_32 w

    -- | Mix an arbitrary sized unboxed array and return the new state
    hashMixBytes :: A.PrimType e => UArray e -> st -> st
    hashMixBytes ba st = A.foldl' (flip hashMix8) st (A.unsafeRecast ba)

unWord16 :: Word16 -> (# Word8, Word8 #)
unWord16 w = (# Prelude.fromIntegral (w `unsafeShiftR` 8)
             ,  Prelude.fromIntegral w #)
{-# INLINE unWord16 #-}

unWord32 :: Word32 -> (# Word8, Word8, Word8, Word8 #)
unWord32 w = (# Prelude.fromIntegral (w `unsafeShiftR` 24)
             ,  Prelude.fromIntegral (w `unsafeShiftR` 16)
             ,  Prelude.fromIntegral (w `unsafeShiftR` 8)
             ,  Prelude.fromIntegral w #)
{-# INLINE unWord32 #-}

unWord64_32 :: Word64 -> (# Word32, Word32 #)
unWord64_32 w = (# Prelude.fromIntegral (w `unsafeShiftR` 32)
                ,  Prelude.fromIntegral w #)
{-# INLINE unWord64_32 #-}
