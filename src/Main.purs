module Main where

import Prelude
import Data.Array ((..))
import Effect (Effect)
import Effect.Console (log)
import Guardition (guardition)

main :: Effect Unit
main = do
  let
    xs = 1 .. 30

    f =
      { l: (>) 10
      , m: (==) 15
      , h: (<) 20
      }

    res = guardition xs f
  log $ show res
