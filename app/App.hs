{-# LANGUAGE TemplateHaskell #-}

module App (main) where

import           Application
import           KatipHandler
import           BuildInfo                       (protoHash)

import           Control.Exception               (bracket)
import           Control.Lens                    ((^.))
import           Control.Lens.Iso.Extended       (stextiso)
import           Control.Monad.Trans.Reader      (runReaderT)
import           Data.Monoid.Colorful            (hGetTerm)
import           Database.Groundhog.Postgresql   (createPostgresqlPool)
import           Katip
import           System.IO                       (stdout)

main :: IO ()
main =
    do
      term <- hGetTerm stdout
      cm <- createPostgresqlPool "" 50
      let appEnv = KatipEnv term cm
      std <- mkHandleScribe ColorIfTerminal stdout DebugS V3
      let mkNm = Namespace [("<" ++ $(protoHash) ++ ">")^.stextiso]
      env <- initLogEnv mkNm "production"
      let env' = registerScribe "stdout" std defaultScribeSettings env
      let runApp le = runKatipContextT le (mempty :: LogContexts) mempty app
      bracket env' closeScribes ((`runReaderT` appEnv) . runApp)