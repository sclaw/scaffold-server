{-# LANGUAGE RecordWildCards #-}

module Katip.Core.Extended (module Katip.Core, logItemIO) where

import           Control.Concurrent (myThreadId)
import           Control.Monad      (when)
import           Data.Foldable      (for_)
import           Data.Map           as M
import           Katip.Core
import           Language.Haskell.TH.Syntax (Loc)

-- katip doesn't provide method that allows
-- to perform logging being living inside IO monad
-- to fix such a omission we need to write
-- function same as logItem
-- but performing in IO
logItemIO
  :: (LogItem a)
  => LogEnv     -- ^ Environment.
  -> Maybe Loc
  -> a          -- ^ Metadata to log.
  -> Namespace  -- ^ Namespace
  -> Severity   -- ^ Message severity.
  -> Severity   -- ^ Minimal interesting severity.
  -> LogStr     -- ^ Textual message.
  -> IO ()
logItemIO LogEnv{..} loc a ns sev minSev msg =
  when (sev >= minSev) $ do
    item <- Item
      <$> return _logEnvApp
      <*> return _logEnvEnv
      <*> return sev
      <*> (mkThreadIdText <$> myThreadId)
      <*> return _logEnvHost
      <*> return _logEnvPid
      <*> return a
      <*> return msg
      <*> _logEnvTimer
      <*> return (_logEnvApp <> ns)
      <*> return loc
    for_ (M.elems _logEnvScribes) $ \(ScribeHandle h _) -> liPush h item
