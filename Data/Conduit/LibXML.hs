{-# LANGUAGE FlexibleContexts #-}

-----------------------------------------------------------------------------
-- |
-- Module: Data.Conduit.LibXML
-- Copyright: 2012 Alpha Heavy Industries
-- License: MIT
--
-- Maintainer: Steve Severance (sseverance@alphaheavy.com)
-- Based on libxml-enumerator by John Millikin
-----------------------------------------------------------------------------

module Data.Conduit.LibXML(parseBytesIO) where

import Control.Exception (ErrorCall(..))
import Control.Monad (unless)
import Control.Monad.Trans
import Control.Monad.Trans.Resource
import Data.ByteString (ByteString)
import Data.Conduit (await,Conduit,yield)
import qualified Data.IORef as IO
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.XML.Types as X
import qualified Text.XML.LibXML.SAX as SAX

setCallbacks :: Monad m => Bool -> SAX.Parser m -> (X.Event -> m Bool) -> m ()
setCallbacks expandRefs p addEvent = do
  let set cb st = SAX.setCallback p cb st
  set SAX.parsedBeginDocument (addEvent X.EventBeginDocument)
  set SAX.parsedEndDocument (addEvent X.EventEndDocument)
  set SAX.parsedBeginElement ((addEvent .) . X.EventBeginElement)
  set SAX.parsedEndElement (addEvent . X.EventEndElement)
  set SAX.parsedCharacters (addEvent . X.EventContent . X.ContentText)
  set SAX.parsedCDATA (addEvent . X.EventCDATA)
  set SAX.parsedComment (addEvent . X.EventComment)
  set SAX.parsedInstruction (addEvent . X.EventInstruction)
  set SAX.parsedExternalSubset ((addEvent .) . X.EventBeginDoctype)
  unless expandRefs (set SAX.parsedReference (addEvent . X.EventContent . X.ContentEntity))

parseBytesIO :: (MonadResource m, MonadIO m) => Bool -> Maybe Text -> Conduit ByteString m X.Event
parseBytesIO expandRefs name = do
  p <- liftIO (SAX.newParserIO name)
  -- error handling
  errRef <- liftIO (IO.newIORef Nothing)
  liftIO (SAX.setCallback p SAX.reportError $ \msg -> do
    print msg
    IO.writeIORef errRef (Just msg)
    return False)
  -- event storage
  eventRef <- liftIO (IO.newIORef [])
  let addEvent e = IO.modifyIORef eventRef (e:) >> return True
  liftIO (setCallbacks expandRefs p addEvent)
  let withEvents io = liftIO $ do IO.writeIORef eventRef []
                                  IO.writeIORef errRef Nothing
                                  void io
                                  events <- IO.readIORef eventRef
                                  err <- IO.readIORef errRef
                                  return (reverse events, err)
  let parseChunk bytes = withEvents (SAX.parseBytes p bytes)
      complete = (withEvents (SAX.parseComplete p))
  runParser parseChunk complete

runParser :: (MonadResource m, MonadIO m, Show a) => (a -> m ([X.Event], Maybe Text)) -> m ([X.Event], Maybe Text) -> Conduit a m X.Event
runParser parseChunk parseComplete = do
  val <- await
  case val of
    Just x -> do
      l <- checkEvents (parseChunk x)
      mapM_ yield l
      runParser parseChunk parseComplete
    Nothing -> do
      l <- checkEvents parseComplete
      mapM_ yield l
      return ()
  where
    checkEvents getEvents = do
      (events, maybeErr) <- lift getEvents
      if null events then
          case maybeErr of
            Nothing -> return [] --runParser parseChunk parseComplete -- continue
            Just x -> lift $ monadThrow $ ErrorCall $ T.unpack x
        else return events

void :: Functor m => m a -> m ()
void = fmap (return ())
