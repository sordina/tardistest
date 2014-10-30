
{-# LANGUAGE RecursiveDo #-}

module Main where

import Control.Monad
import Control.Monad.Tardis

print_tardis_example :: IO ()
print_tardis_example = print $ runTardis tardis_example (0,0)

tardis_example = do
  x <- getFuture
  sendFuture 234
  sendPast   123
  y <- getPast
  return (x + y)
