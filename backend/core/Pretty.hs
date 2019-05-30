module Pretty (mkPretty, pPrint) where
      
import Text.Pretty.Simple
import Data.Text.Lazy (toStrict)
import Control.Lens ((^.), to, from)
import Control.Lens.Iso.Extended (stextiso)

mkPretty :: Show a => String -> a -> String
mkPretty msg x = msg ++ "\n" ++ pShowOpt defaultOutputOptionsNoColor x^.to toStrict.from stextiso