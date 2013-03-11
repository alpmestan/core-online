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
    simpleHTTP nullConf (coreApp conn)

myPolicy :: BodyPolicy
myPolicy = defaultBodyPolicy "/tmp/" 0 1000 1000

coreApp :: Connection -> ServerPart Response
coreApp conn = do
    decodeBody myPolicy
    msum [ dir "static" $ serveDirectory EnableBrowsing ["index.html"] "./static"
         , dir "core"   $ path $ \coreId -> viewCoreH False coreId conn
         , dir "add"    $ do methodM POST
                             addCoreH conn
         , dir "about"  $ about
         , home conn
         ]
