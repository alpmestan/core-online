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

ghcBinsDir :: FilePath
ghcBinsDir = "/home/alpmestan/haskell/ghc/bin"

ghcCoreFor :: Text -> Int64 -> Text -> Text -> Text -> IO Text
ghcCoreFor ghcVer cid haskellCode optlevel mName = do
	T.writeFile hsFilePath haskellModule
	(exitStatus, out, err) <- readProcessWithExitCode ghc args ""
	if exitStatus == ExitSuccess then return . cleanup $ T.pack out else let (ExitFailure code) = exitStatus in return $ failureMsg code out err
        
    where failureMsg code out err = "GHC failed to compile, exit code: " `T.append`
              (T.pack . show $ code) `T.append` "\n" `T.append` (T.pack out) `T.append` (T.pack err)
          args = words $ "-c " ++ T.unpack optlevel ++ " " ++ hsFilePath ++ " -ddump-simpl -dsuppress-idinfo -dsuppress-coercions -dsuppress-type-applications -dsuppress-uniques -dsuppress-module-prefixes"
          ghc  = ghcBinsDir </> ("ghc-" ++ T.unpack ghcVer)
          hsFilePath = ghcCoreDir </> hsFileName
          hsFileName = if mName == T.empty then ("M" ++ show cid) <.> "hs" else T.unpack mName <.> "hs"
          haskellModule = if mName == T.empty 
                              then T.pack ("module M" ++ show cid ++ " where \n") `T.append` haskellCode
                              else haskellCode
          printInfos a b c d = T.putStrLn a >> T.putStrLn b >> T.putStrLn c >> T.putStrLn d
          cleanUp = T.reverse . T.tail . T.tail . T.dropWhile (/= '\n') . T.tail . T.dropWhile (/= '\n') . T.tail . T.dropWhile (/= '\n') . T.reverse . T.tail . T.dropWhile (/= '\n') . T.tail . T.dropWhile (/= '\n') . T.tail . T.dropWhile (/= '\n')

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