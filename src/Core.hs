{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Core where

import Control.Applicative
import Control.Monad.Trans (liftIO)
import Data.Int
import Data.Text (Text)
import Database.SQLite.Simple

data Core = Core 
    { coreId       :: Int64
    , coreAuthor   :: Text
    , coreTitle    :: Text
    , coreHS       :: Text
    , coreCore     :: Text
    , coreOptLevel :: Text
    } deriving Show

instance FromRow Core where
	fromRow = Core <$> field <*> field <*> field <*> field <*> field <*> field

instance ToRow Core where
	toRow (Core{..}) = toRow (coreId, coreAuthor, coreTitle, coreHS, coreCore, coreOptLevel)
    
getNextCoreId :: Connection -> IO Int64
getNextCoreId conn = do
    (cids :: [Only Int64]) <- liftIO $ query_ conn "select id from cores order by id desc limit 1"
    return $ if cids == [] then 1 else fromOnly (head cids) + 1

getNumberOfCores :: Connection -> IO Int64
getNumberOfCores conn = do
    [Only nbCores] <- liftIO $ query_ conn "select count(id) from cores"
    return nbCores

insertCore :: Connection -> Core -> IO ()
insertCore conn coreData = execute conn "insert into cores values (?, ?, ?, ?, ?, ?)" coreData

getCoreById :: Connection -> Int64 -> IO (Maybe Core)
getCoreById conn cId = do
    res <- liftIO $ query conn "select * from cores where id = ?" (Only (cId :: Int64))
    case res of
        []    -> return Nothing
        (c:_) -> return (Just c)