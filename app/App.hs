{-# LANGUAGE TemplateHaskell #-}

module App (main) where

import           Application
import           KatipHandler
import           BuildInfo                       (protoHash)
import           Config

import           Control.Exception               (bracket)
import           Control.Lens                    ((^.))
import           Control.Lens.Iso.Extended       (stextiso)
import           Control.Monad.Trans.Reader      (runReaderT)
import           Data.Monoid.Colorful            (hGetTerm)
import           Database.Groundhog.Postgresql   (createPostgresqlPool)
import           Katip
import           System.IO                       (stdout)
import qualified Data.Pool  as                   Pool
import qualified Hasql.Pool as                   Hasql 
import           System.Environment              (getArgs)


main :: IO ()
main =
    do
      (path:_) <- getArgs
      cfg <- Config.load path
      term <- hGetTerm stdout
      orm <- createPostgresqlPool "" 50
      raw <- Pool.createPool (Hasql.acquire (50, 10000, "")) Hasql.release 1 100 1  
      let appEnv = KatipEnv term orm raw
      std <- mkHandleScribe ColorIfTerminal stdout DebugS V3
      let mkNm = Namespace [("<" ++ $(protoHash) ++ ">")^.stextiso]
      env <- initLogEnv mkNm "production"
      let env' = registerScribe "stdout" std defaultScribeSettings env
      let runApp le = 
            runKatipContextT le 
            (mempty :: LogContexts) 
            mempty 
            (run (cfg^.ports.port))
      bracket env' closeScribes ((`runReaderT` appEnv) . runApp)