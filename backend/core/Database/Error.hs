module Database.Error (Error) where 

import Control.Exception.Base

data Error = Some deriving Show

instance Exception Error