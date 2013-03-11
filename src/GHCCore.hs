{-# LANGUAGE OverloadedStrings #-}

module GHCCore (ghcCoreFor, ghcVersions) where

import Data.Int    
import Data.Text (Text)
import System.Exit
import System.FilePath
import System.IO.Unsafe
import System.Process

import qualified Data.Text    as T
import qualified Data.Text.IO as T

ghcCoreDir :: FilePath
ghcCoreDir = "./hs/"

ghcCoreFor :: Int64 -> Text -> IO Text
ghcCoreFor cid haskellCode = do
	T.writeFile hsFilePath haskellModule
	(exitStatus, out, err) <- readProcessWithExitCode ghc args ""
	if exitStatus == ExitSuccess then return $ T.reverse . T.tail . T.tail . T.dropWhile (/= '\n') . T.tail . T.dropWhile (/= '\n') . T.tail . T.dropWhile (/= '\n') . T.reverse . T.tail . T.dropWhile (/= '\n') . T.tail . T.dropWhile (/= '\n') . T.tail . T.dropWhile (/= '\n') $ T.pack out else let (ExitFailure code) = exitStatus in return $ failureMsg code out err
        
    where failureMsg code _ err = "GHC failed to compile, exit code: " `T.append`
              (T.pack . show $ code) `T.append` "\n" `T.append` (T.pack err)
          args = words $ "-c -O2 " ++ hsFilePath ++ " -ddump-simpl -dsuppress-idinfo -dsuppress-coercions -dsuppress-type-applications -dsuppress-uniques -dsuppress-module-prefixes"
          ghc  = "ghc"
          hsFilePath = ghcCoreDir </> hsFileName
          hsFileName = ("M" ++ show cid) <.> "hs"
          haskellModule = T.pack ("module M" ++ show cid ++ " where \n") `T.append` haskellCode

ghcVersions :: [Text]
ghcVersions = [ "7.6.2"
              , "7.6.1"
              , "7.4.2"
              , "7.4.1"
              , "7.2.2"
              , "7.2.1"
              , "7.0.4"
              , "7.0.3"
              , "7.0.2"
              , "7.0.1"
              , "6.12.3" ]