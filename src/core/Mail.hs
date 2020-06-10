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
      for template_e $ \tmpl -> do
        let payloadHtml = toS $ Ginger.htmlSource $ Ginger.easyRender @Ginger.Html msg tmpl
        let payloadText = mempty
        mail <-
          Network.Mail.Mime.simpleMail
          (Network.Mail.Mime.Address Nothing to_whom)
          (Network.Mail.Mime.Address Nothing "info@edgenode.org")
          (mkSubject ty)
          payloadText
          payloadHtml
          mempty
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
loadGingerFile path = do
  res_e <- tryIOError $
    openFile path ReadMode >>=
    hGetContents
  case res_e of
    Right contents -> return (Just contents)
    Left _ -> return Nothing

mkSubject :: Type -> T.Text
mkSubject TypeNewProviderAccount = "Account"
mkSubject TypeResetPassword = "Reset password"
mkSubject TypeNewPassword = "New Password"