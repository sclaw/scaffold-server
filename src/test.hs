
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE FlexibleContexts #-}
import qualified Text.RE.PCRE as Regexp
import qualified Text.RE.Replace as Regexp
import Debug.Trace

s :: String
s = "(\"countries\",Array [String \"russia\",String \"france\"])"

countriesPattern = [Regexp.re|(?<=Array\s\[String\s\"|,String\s\")[a-z_]*(?=\"|\"\])|]

go pattern transform src =
      Regexp.replaceAllCaptures
      Regexp.ALL
      (replace transform) $
      src Regexp.*=~ pattern
  where
      replace transform _ loc cap = Just $
        case Regexp.locationCapture loc of
          0 -> transform $ Regexp.capturedText cap
          _ -> error "replaceEnum:go"

t  = go countriesPattern r s

r s =  "111"

