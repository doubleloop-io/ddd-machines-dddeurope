module Main where

import           DDD                   (runApplication)
import           Door                  (DoorCommand (..), doorApplication)

-- base
import           Data.Functor.Identity (runIdentity)

main :: IO ()
main =
  -- run the whole computation without effect
  print . runIdentity $
    runApplication
      doorApplication -- machines combine into one
      [Open, Open, Open, Close, Close, Knock] -- commands

  -- prints: 2
  -- prints: (1 for event produced by explicit Open command)
  --        + (1 for event produced by KnockPolicy emitted Open command)
