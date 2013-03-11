{-# LANGUAGE OverloadedStrings #-}

module Templates ( 
                   template
                 , footer
                 , selectFor
                 , inputTextFor
                 ) where
import GHCCore

import Data.List (intersperse)
import Data.Text (Text)
import Text.Blaze ((!), toValue)
import qualified Text.Blaze.Html4.Strict as H
import qualified Text.Blaze.Html4.Strict.Attributes as A

template :: Text -> [H.Html] -> H.Html -> H.Html
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
    (H.a ! A.href "/about" $ "About")
    >> 
    " - "
    >>
    (H.a ! A.href "http://github.com/alpmestan/core-online" $ "Code on github")
    >>
    " - "
    >>
    (H.a ! A.href "http://stackoverflow.com/a/6121495/193424" $ "Reading GHC Core (Stack Overflow)")
    >> H.br >>
    ("Running GHC " >> (sequence_ . map H.toHtml . intersperse ", " $ ghcVersions))

selectFor :: Text -> Text -> [ (Text, Text) ] -> H.Html
selectFor label name opts = do 
    H.label ! A.for (toValue name) $ H.toHtml label
    H.select ! A.name (toValue name) $ 
        sequence_ $ map (\(optLabel, optValue) -> H.option ! A.value (toValue optValue) $ H.toHtml optLabel) opts

inputTextFor :: Text -> Text -> H.Html
inputTextFor label name = 
    (H.label ! A.for (toValue name) $ H.toHtml label) >> (H.input ! A.type_ "text" ! A.id (toValue name) ! A.name (toValue name)) 