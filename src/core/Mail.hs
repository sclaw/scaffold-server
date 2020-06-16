{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}

module Mail (run) where

import EdgeNode.Mail
import EdgeNode.Statement.Mail as Mail
import EdgeNode.Config

import KatipController
import Katip
import Control.Concurrent
import Database.Transaction
import Data.Pool
import Hasql.Connection
import Control.Monad
import Control.Monad.IO.Class
import Data.Traversable
import Data.String.Conv
import qualified Network.Mail.Mime
import Mail.Smtp
import qualified Text.Ginger.Parse as Ginger
import TH.Proto
import System.Directory
import System.FilePath.Posix
import System.IO (IOMode(ReadMode), openFile, hGetContents)
import System.IO.Error (tryIOError)
import qualified Text.Ginger.Run as Ginger
import qualified Text.Ginger.Html as Ginger
import qualified Data.Text as T
import Data.Functor
import Data.Either
import qualified Data.Vector as V
import Data.Foldable
import Web.Telegram
import Pretty
import BuildInfo
import Data.Either.Combinators

run :: Smtp -> Web.Telegram.Service -> Pool Connection -> KatipLoggerIO -> IO ()
run smtp@Smtp {..} telegram pool logger = do
  logger InfoS $ ls @String "mailing service starts..."
  dir <- getCurrentDirectory
  void $ transaction pool logger $ do
    xs <- statement Mail.getMails StatusNew
    liftIO $ logger InfoS $ ls $ "mail to be sent: " <> show xs
    xs_e <- liftIO $ for xs $ \(ident, ty, to_whom, msg) -> do
      let path = dir </> "template" </> "email" </> fromType ty
      template_e <- Ginger.parseGingerFile loadGingerFile path
      let subject_path = dir </> "template" </> "email" </> "subject" </> fromType ty
      subject <- mkSubject logger telegram smtpDefaultSubject subject_path
      for template_e $ \tmpl -> do
        let payloadHtml = toS $ Ginger.htmlSource $ Ginger.easyRender @Ginger.Html msg tmpl
        let payloadText = mempty
        mail <- Network.Mail.Mime.simpleMail
          (Network.Mail.Mime.Address Nothing to_whom)
          (Network.Mail.Mime.Address Nothing smtpEmail)
          subject payloadText payloadHtml mempty
        logger DebugS $ ls @String $ show mail
        send telegram logger $ toS $ mkPretty ("At module " <> $location) mail
        sendMailTlsDefPort (toS smtpServer) (toS smtpLogin) (toS smtpPassword) mail $> ident
    let (errs, idents) = partitionEithers $ V.toList xs_e
    liftIO $ for_ errs $ \e -> do
      send telegram logger $ toS $ mkPretty ("At module " <> $location) e
      logger ErrorS $ ls @String $ show e
    statement Mail.setAsProcessed (StatusProcessed, V.fromList idents)
  threadDelay (30 * 10 ^ 6)
  run smtp telegram pool logger

loadGingerFile :: FilePath -> IO (Maybe String)
loadGingerFile path = fmap (either (const Nothing) Just) $ tryIOError $ openFile path ReadMode >>= hGetContents

mkSubject :: KatipLoggerIO -> Web.Telegram.Service -> T.Text -> FilePath -> IO T.Text
mkSubject logger telegram default_subject path = do
  subject_e <- tryIOError $ openFile path ReadMode >>= hGetContents
  whenLeft subject_e $ \e -> do
    logger InfoS $ ls @String $ "subject not found. use default one. " <> show e
    send telegram logger $ toS $ mkPretty ("At module " <> $location) ("subject not found. use default one. " <> show e)
  pure $ either (const default_subject) toS subject_e