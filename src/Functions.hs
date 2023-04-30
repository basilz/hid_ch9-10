module Functions
  ( isPrime,
    isPrimeOpt,
    buildIPFoldr,
    buildIPFoldl,
    buildIPFoldlShl,
    IP(..)
  )
where
import Data.Word ( Word8, Word32 )
import Data.Bits ( Bits(shiftL) )

isPrime :: Integer -> Bool
isPrime n = all notDividedBy [2 .. n]
  where
    notDividedBy m = n `mod` m /= 0

isPrimeOpt :: Integer -> Bool
isPrimeOpt n = all notDividedBy [2 .. n `div` 2]
  where
    notDividedBy m = n `mod` m /= 0


newtype IP = IP {unIP :: Word32}
  deriving (Eq, Ord)

buildIPFoldr :: [Word8] -> IP
buildIPFoldr = IP . fst . foldr go (0, 1)
  where
    go b (s, k) = (s + fromIntegral b * k, k*256)

buildIPFoldl :: [Word8] -> IP
buildIPFoldl = IP . foldl (\s b -> s * 256 + fromIntegral b) 0

buildIPFoldlShl :: [Word8] -> IP
buildIPFoldlShl = IP . foldl (\s b -> shiftL s 8 + fromIntegral b) 0