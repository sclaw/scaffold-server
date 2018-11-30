module App where

import Logger (init)
import Prelude hiding (init)

main :: IO ()
main = 
    do
      _ <- init "log"
      print "run app"