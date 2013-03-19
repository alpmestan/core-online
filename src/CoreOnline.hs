{-# LANGUAGE OverloadedStrings #-}

module Main where

import About
import AddCore
import Home
import ViewCore

import Control.Monad
import Database.SQLite.Simple
import Happstack.Server

main :: IO ()
main = withConnection "db/cores.db" $ \conn -> do
    simpleHTTP myConf (coreApp conn)

myConf :: Conf
myConf = nullConf { port = 31336 }

myPolicy :: BodyPolicy
myPolicy = defaultBodyPolicy "/tmp/" 0 1000 1000

coreApp :: Connection -> ServerPart Response
coreApp conn = do
    decodeBody myPolicy
    msum [ dir "static" $ serveDirectory EnableBrowsing ["index.html"] "./static"
         , dir "about"  $ about
         , dir "add"    $ do methodM POST
                             addCoreH conn
         , path $ \coreId -> viewCoreH False coreId conn
         , home conn
         ]
