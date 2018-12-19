module Pretty (mkPretty) where
      
import Text.Pretty.Simple (pShowOpt, defaultOutputOptionsNoColor)
import Data.Text.Lazy (toStrict)
import Control.Lens ((^.), to, from)
import Control.Lens.Iso.Extended (stextiso)

mkPretty :: Show a => String -> a -> String
mkPretty msg x = msg ++ "\n" ++ pShowOpt defaultOutputOptionsNoColor x^.to toStrict.from stextiso