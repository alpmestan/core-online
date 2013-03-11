{-# LANGUAGE OverloadedStrings #-} 

module AddCore (addCoreH) where

import Core
import GHCCore
import ViewCore

import Control.Monad.Trans (liftIO)
import Database.SQLite.Simple
import Happstack.Server
import Text.Blaze.Html.Renderer.Text (renderHtml)
import Text.Highlighter
import Text.Highlighter.Formatters.Html
import Text.Highlighter.Lexers.Haskell (lexer)

import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import qualified Data.Text.Lazy as LT

addCoreH :: Connection -> ServerPart Response
addCoreH conn = do
    author         <- lookText' "author"
    title          <- lookText' "title"
    optlevel       <- lookText' "optlevel"
    haskell        <- (`T.append` "\n") `fmap` lookText' "haskell"
    cId            <- liftIO $ getNextCoreId conn
    core           <- liftIO $ ((`T.append` "\n") . T.tail . T.dropWhile (/='\n')) `fmap` (ghcCoreFor cId haskell) 
    let (Right tokensHaskell) = runLexer lexer $ (T.encodeUtf8 haskell) -- True: we want line numbers
    let (Right tokensCore)    = runLexer lexer $ (T.encodeUtf8 core)    -- same here
    let eHaskell = LT.toStrict . renderHtml . format True $ tokensHaskell
    let eCore    = LT.toStrict . renderHtml . format True $ tokensCore
    let coreData = Core cId author title eHaskell eCore optlevel 
    liftIO $ insertCore conn coreData
    viewCoreH True cId conn