{-# LANGUAGE TemplateHaskell #-}

module App (main) where

import           Application
import           BuildInfo                       (protoHash)
import           Logger

import           Control.Exception               (bracket)
import           Control.Lens                    ((^.))
import           Control.Lens.Iso.Extended       (stextiso)
import           Control.Monad.Trans.Reader      (runReaderT)
import           Data.Monoid.Colorful            (hGetTerm)
import           Data.Time.Clock                 (getCurrentTime)
import           Database.Groundhog.Postgresql   (createPostgresqlPool)
import           Katip
import           System.IO                       (stdout)

main :: IO ()
main =
    do
      term <- hGetTerm stdout
      cm <- createPostgresqlPool "" 50
      let appEnv = AppEnv term cm
      std <- mkHandleScribe ColorIfTerminal stdout DebugS V0
      now <- getCurrentTime
      path <- getPath "log" now
      file <- mkFileScribe path DebugS V2
      let mkNm = Namespace [("<" ++ $(protoHash) ++ ">")^.stextiso]
      env <- initLogEnv mkNm "production"
      env' <- registerScribe "stdout" std defaultScribeSettings env
      let env'' = registerScribe "file" file defaultScribeSettings env'
      let runApp le = runKatipContextT le (mempty :: LogContexts) mempty app
      bracket env''
       closeScribes
       ((`runReaderT` appEnv) . runApp)