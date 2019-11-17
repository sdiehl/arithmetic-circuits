{-# LANGUAGE NoImplicitPrelude #-}

-- To get the benchmarking data, run "stack bench".

module Main where

import Protolude

import Criterion.Main

import qualified Circuit

main :: IO ()
main = defaultMain
      [ bgroup "Circuit to QAP translation" Circuit.benchmarks
      ]
