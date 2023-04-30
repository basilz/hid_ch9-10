{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}

import Criterion.Main
import Functions
import GHC.Generics (Generic)
import Control.DeepSeq (NFData)

deriving instance Generic IP
deriving instance NFData IP

benchPrime :: [Benchmark]
benchPrime =
  [ let primeNumber = 1683
     in bgroup
          "prime"
          [ bench "isPrime (naive)" $ whnf isPrime primeNumber,
            bench "isPrime (opt)" $ whnf isPrimeOpt primeNumber
          ]
  ]

benchBuildIP :: [Benchmark]
benchBuildIP =
  [ bgroup
      "buildIP"
      [ let theIP = [17, 0, 32, 2]
         in bgroup
              "single"
              [ bench "buildip-foldr" $ whnf buildIPFoldr theIP,
                bench "buildip-foldl" $ whnf buildIPFoldl theIP,
                bench "buildip-foldl-shl" $ whnf buildIPFoldlShl theIP
              ],
        let ipcomps = [[0, 0, 0, 1], [192, 168, 1, 1], [255, 255, 252, 41], [17, 0, 32, 2]]
         in bgroup
              "several"
              [ bench "buildip-foldr" $ nf (map buildIPFoldr) ipcomps,
                bench "buildip-foldl" $ nf (map buildIPFoldl) ipcomps,
                bench "buildip-foldl-shl" $ nf (map buildIPFoldlShl) ipcomps
              ]
      ]
  ]

main :: IO ()
main = defaultMain benchBuildIP
