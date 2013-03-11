module Home (home) where

import Core
import Templates.Home

import Control.Monad.Trans (liftIO)
import Database.SQLite.Simple
import Happstack.Server

home :: Connection -> ServerPart Response
home conn = do
    nbCores <- liftIO $ getNumberOfCores conn
    ok . toResponse $ homeHtml nbCores
            