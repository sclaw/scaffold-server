{-# LANGUAGE TemplateHaskell #-}

module App (main) where

import           Application
import           BuildInfo                       (protoHash)
import           Logger

import           Control.Concurrent.Async.Lifted (race_)
import           Control.Concurrent.MVar         (MVar, newEmptyMVar, takeMVar)
import           Control.Exception               (bracket)
import           Control.Lens                    ((%~), (&), (^.))
import           Control.Lens.Iso.Extended       (stextiso)
import           Control.Monad.IO.Class
import           Control.Monad.Trans.Reader      (runReaderT)
import qualified Data.Map.Strict                 as M
import           Data.Monoid.Colorful            (hGetTerm)
import           Data.Time.Clock                 (UTCTime, getCurrentTime)
import           Database.Groundhog.Postgresql   (createPostgresqlPool)
import           Katip
import           Katip.Core                      (ScribeHandle (shScribe))
import           System.FilePath.Posix           (FilePath)
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
      var <- newEmptyMVar
      let env'' = registerScribe "file" file defaultScribeSettings env'
      let runApp le =
           runKatipContextT le
           (mempty :: LogContexts)
           mempty
           (race_ app (mkNextDayFile now var))
      bracket env''
       closeScribes
       ((`runReaderT` appEnv) . runApp)

mkNextDayFile :: UTCTime -> MVar FilePath -> KatipContextT App ()
mkNextDayFile t0 var = race_ (liftIO (mkNextDayPath "log" t0 var)) (modifyEnv var)

modifyEnv :: MVar FilePath -> KatipContextT App ()
modifyEnv var =
    do
      path <- liftIO $ takeMVar var
      file <- liftIO $ mkFileScribe path DebugS V2
      let replace = M.update (\x -> Just x { shScribe = file }) "file"
      localLogEnv (& logEnvScribes %~ replace) (modifyEnv var)
