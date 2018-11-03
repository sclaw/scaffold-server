module App where

import Logger (init)
import Prelude hiding (init)

main :: IO ()
main = 
    do
      init   
      print "run app"