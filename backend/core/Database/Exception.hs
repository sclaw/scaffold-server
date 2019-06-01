module Database.Exception (Groundhog) where 

import Control.Exception.Base

data Groundhog = Some deriving Show

instance Exception Groundhog