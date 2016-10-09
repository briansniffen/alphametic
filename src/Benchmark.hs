module Benchmark where

import Types
import Alphametic hiding (main)
import Criterion.Main

main = benchmark

benchmark :: IO ()
benchmark = 
  defaultMain [bgroup "" [ bench "akamai" $ nfIO (solveExample "lean + agile + kanban = akamai"),
                           bench "money" $ nfIO (solveExample "send + more = money"),
                           bench "six" $ nfIO (solveExample "six + six + six = nine + nine"),
                           bench "danger" $ nfIO (solveExample "cross + roads = danger"),
                           bench "tales" $ nfIO (solveExample "old + salt + told + tall = tales"),
                           bench "lbj" $ nfIO (solveExample "lyndon * b = johnson")
                         ]
              ]
