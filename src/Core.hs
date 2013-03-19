{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Core where

import Control.Applicative
import Control.Monad.Trans (liftIO)
import Data.Int
import Data.Text (Text)
import Database.SQLite.Simple
import Database.SQLite.Simple.FromField
import Database.SQLite.Simple.ToField

newtype CoreId = CoreId { unCoreId :: Int64 } deriving (Eq, FromField, ToField, Num)

instance Show CoreId where
    show c = show (unCoreId c)

instance Read CoreId where
    readsPrec n s = map (\(a,b) -> (CoreId a, b)) $ readsPrec n s

data Core = Core 
    { coreId       :: CoreId
    , coreAuthor   :: Text
    , coreTitle    :: Text
    , coreHS       :: Text
    , coreCore     :: Text
    , coreOptLevel :: Text
    , coreGhcVer   :: Text
    } deriving Show

instance FromRow Core where
	fromRow = Core <$> field <*> field <*> field <*> field <*> field <*> field <*> field

instance ToRow Core where
	toRow (Core{..}) = toRow (coreId, coreAuthor, coreTitle, coreHS, coreCore, coreOptLevel, coreGhcVer)
    
getNextCoreId :: Connection -> IO CoreId
getNextCoreId conn = do
    (cids :: [Only CoreId]) <- liftIO $ query_ conn "select id from cores order by id desc limit 1"
    return $ if cids == [] then 1 else fromOnly (head cids) + 1

getNumberOfCores :: Connection -> IO CoreId 
getNumberOfCores conn = do
    [Only nbCores] <- liftIO $ query_ conn "select count(id) from cores"
    return nbCores

insertCore :: Connection -> Core -> IO ()
insertCore conn coreData = execute conn "insert into cores values (?, ?, ?, ?, ?, ?, ?)" coreData

getCoreById :: Connection -> CoreId -> IO (Maybe Core)
getCoreById conn cId = do
    res <- liftIO $ query conn "select * from cores where id = ?" (Only cId)
    case res of
        []    -> return Nothing
        (c:_) -> return (Just c)
