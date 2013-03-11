module ViewCore (viewCoreH) where

import Core
import Templates.ViewCore

import Control.Monad.Trans (liftIO)
import Data.Int
import Database.SQLite.Simple
import Happstack.Server

viewCoreH :: Bool -> Int64 -> Connection -> ServerPart Response
viewCoreH justAddedMsg cId conn = liftIO (getCoreById conn cId) >>= f
    
    where f :: Maybe Core -> ServerPart Response
          f = maybe (ok . toResponse $ coreNotFound)
                    (ok . toResponse . coreFound justAddedMsg)