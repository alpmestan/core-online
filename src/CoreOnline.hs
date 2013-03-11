{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Main where

import Control.Applicative
import Control.Monad
import Control.Monad.Trans (liftIO)
import Data.Int
import Data.Text (Text)
import Database.SQLite.Simple
import Happstack.Server
import System.Exit
import System.FilePath
import System.IO
import System.Process
import Text.Blaze ((!), toValue)
import Text.Blaze.Html (preEscapedToHtml)
import Text.Blaze.Renderer.Text (renderHtml)
import Text.Highlighter
import Text.Highlighter.Formatters.Html
import Text.Highlighter.Lexers.Haskell (lexer)

import qualified Data.Text as T
import qualified Data.Text.Lazy as LT
import qualified Data.Text.IO as T
import qualified Data.Text.Encoding as T
import qualified Text.Blaze.Html4.Strict as H
import qualified Text.Blaze.Html4.Strict.Attributes as A

main :: IO ()
main = withConnection "db/cores.db" $ \conn -> do
    simpleHTTP nullConf (coreApp conn)

myPolicy :: BodyPolicy
myPolicy = defaultBodyPolicy "/tmp/" 0 1000 1000

coreApp :: Connection -> ServerPart Response
coreApp conn = do
    decodeBody myPolicy
    msum [ dir "static" $ serveDirectory EnableBrowsing ["index.html"] "./static"
         , dir "core"  $ path $ \coreId -> viewCoreH False coreId conn
         , dir "add"   $ do methodM POST
                            addCoreH conn
         , dir "about" $ about
         , home conn
         ]

addCoreH :: Connection -> ServerPart Response
addCoreH conn = do
    author         <- lookText' "author"
    title          <- lookText' "title"
    optlevel       <- lookText' "optlevel"
    haskell        <- (`T.append` "\n") `fmap` lookText' "haskell"
    (cids :: [Only Int64]) <- liftIO $ query_ conn "select id from cores order by id desc limit 1"
    let cId = if cids == [] then 1 else fromOnly (head cids) + 1
    core           <- liftIO $ ((`T.append` "\n") . T.tail . T.dropWhile (/='\n')) `fmap` (ghcCoreFor cId haskell) 
    let (Right tokensHaskell) = runLexer lexer $ (T.encodeUtf8 haskell) -- True: we want line numbers
    let (Right tokensCore)    = runLexer lexer $ (T.encodeUtf8 core)    -- same here
    let eHaskell = LT.toStrict . renderHtml . format True $ tokensHaskell
    let eCore    = LT.toStrict . renderHtml . format True $ tokensCore
    let coreData = (cId :: Int64, author, title, eHaskell, eCore, optlevel)
    liftIO $ execute conn "insert into cores values (?, ?, ?, ?, ?, ?)" coreData
    viewCoreH True cId conn
    
viewCoreH :: Bool -> Int64 -> Connection -> ServerPart Response
viewCoreH justAddedMsg cId conn = do
	[core] <- liftIO $ query conn "select * from cores where id = ?" (Only (cId :: Int64))
	ok . toResponse $ template ("you want to see core #" ++ show cId)
							   [H.link ! A.rel "stylesheet" ! A.type_ "text/css" ! A.href "/static/highlight.css"]
							   (when justAddedMsg (H.div ! A.class_ "msg" $ "Successfully added core #" >> H.toHtml (show cId))
                                >>
                                (H.p . H.toHtml $ "core #" `T.append` (T.pack $ show cId))
							    >> 
							    htmlForCore core)
htmlForCore :: Core -> H.Html        
htmlForCore (Core{..}) = H.div ! A.class_ "container" $ do
    H.h2 . H.toHtml $ coreTitle
    H.h3 $ "by: " >> H.toHtml coreAuthor
    H.h3 $ "optlevel: " >> H.toHtml coreOptLevel
    H.p ! A.class_ "paste" ! A.id "haskell" $ preEscapedToHtml coreHS
    H.p ! A.class_ "paste" ! A.id "core" $ preEscapedToHtml coreCore

about :: ServerPart Response
about = ok . toResponse $ template "about" 
                                   []
                                   (H.p "about...!?")

home :: Connection -> ServerPart Response
home conn = do
    [Only nbCores] <- liftIO $ query_ conn "select count(id) from cores"
    ok . toResponse $ 
            template "bonsouare"
                      []
                      ((H.h1 ! A.class_ "header" $ "core-online - get the GHC Core for your haskell code and share it")
                       >>
                       (H.p $ coreForm)
                       >>
                       (H.p ! A.class_ "footer" $ do
                           "The database currently contains "
                           >>
                           (H.b . H.toHtml $ show (nbCores :: Int64))
                           >> 
                           " cores.")
                       )

template :: String -> [H.Html] -> H.Html -> H.Html
template title headers body = 
	H.html $ do
		H.head $ do
			H.title (H.toHtml title) >> H.link ! A.href "/static/style.css" ! A.rel "stylesheet" ! A.type_ "text/css"
			H.meta ! A.httpEquiv "Content-Type" ! A.content "text/html;charset=utf-8"
			sequence_ headers
		H.body $ body >> footer
        
footer :: H.Html
footer = H.p ! A.class_ "footer" $ do
    (H.a ! A.href "/" $ "Home")
    >>
    " - "
    >> 
    (H.a ! A.href "http://github.com/alpmestan/core-online" $ "Code on github")
    >>
    " - "
    >>
    (H.a ! A.href "http://stackoverflow.com/a/6121495/193424" $ "Reading GHC Core (Stack Overflow)")

coreForm :: H.Html
coreForm = H.p ! A.class_ "core-form" $ H.form ! A.action "/add" ! A.enctype "multipart/form-data" ! A.method "POST" $ do
    H.h2 ! A.class_ "formHead" $ "Generate GHC Core for"
    H.p ! A.class_ "author" $ inputTextFor "Author" "author"
    H.p ! A.class_ "title" $ inputTextFor "Title"  "title"
    H.p ! A.class_ "haskell" $ H.textarea ! A.name "haskell" ! A.rows "50" ! A.cols "100" $ "Your Haskell code here."
    H.p ! A.class_ "optlevel" $ selectFor "Optimization Level" 
                                          "optlevel" 
                                          [ ("No optimizations: -O0", "-O0")
                                          , ("Basic optimizations: -O1", "-O1")
                                          , ("Agressive optimizations: -O2", "-O2")
                                          ]
    H.p ! A.class_ "submit" $ H.input ! A.type_ "submit" ! A.value "Submit"

selectFor :: Text -> Text -> [ (Text, Text) ] -> H.Html
selectFor label name opts = do 
    H.label ! A.for (toValue name) $ H.toHtml label
    H.select ! A.name (toValue name) $ 
        sequence_ $ map (\(optLabel, optValue) -> H.option ! A.value (toValue optValue) $ H.toHtml optLabel) opts

inputTextFor :: Text -> Text -> H.Html
inputTextFor label name = 
    (H.label ! A.for (toValue name) $ H.toHtml label) >> (H.input ! A.type_ "text" ! A.id (toValue name) ! A.name (toValue name)) 
    
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

ghcCoreFor :: Int64 -> Text -> IO Text
ghcCoreFor cid haskellCode = do
	T.writeFile hsFileName haskellModule
	(exitStatus, out, err) <- readProcessWithExitCode ghc args ""
	if exitStatus == ExitSuccess then return . T.pack $ out else let (ExitFailure code) = exitStatus in return $ failureMsg code out err
        
    where failureMsg code out err = "GHC failed to compile, exit code: " `T.append`
              (T.pack . show $ code) `T.append` "\n" `T.append` (T.pack err)
          args = words $ "-c -O2 " ++ hsFileName ++ " -ddump-simpl -dsuppress-idinfo -dsuppress-coercions -dsuppress-type-applications -dsuppress-uniques -dsuppress-module-prefixes"
          ghc  = "ghc-7.6.1"
          hsFileName = ("M" ++ show cid) <.> "hs"
          haskellModule = T.pack ("module M" ++ show cid ++ " where \n") `T.append` haskellCode
