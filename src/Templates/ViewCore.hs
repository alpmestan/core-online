{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Templates.ViewCore where

import Core
import Templates

import Control.Monad (when)
import Text.Blaze ((!), toValue)
import Text.Blaze.Html (preEscapedToHtml)

import qualified Data.Text as T
import qualified Text.Blaze.Html4.Strict as H
import qualified Text.Blaze.Html4.Strict.Attributes as A

htmlForCore :: Core -> H.Html        
htmlForCore (Core{..}) = H.div ! A.class_ "container" $ do
    H.h1 ! A.class_ "coreTitle" $ H.a ! A.href (toValue $ "/core/" ++ show coreId) $ H.toHtml coreTitle
    H.p ! A.class_ "coreDesc" $ do
        (H.span ! A.class_ "coreDescLabel" $ "by: ")
        >>
        H.i (H.toHtml coreAuthor)
        >>
        H.br
        >>
        (H.span ! A.class_ "coreDescLabel" $ "optimization level: ")
        >>
        H.i (H.toHtml coreOptLevel)
    H.p ! A.class_ "paste" ! A.id "haskell" $ preEscapedToHtml coreHS
    H.p ! A.class_ "paste" ! A.id "core" $ preEscapedToHtml coreCore
    
coreNotFound :: H.Html
coreNotFound = template "Not found" [] ("the core you are looking for doesn't exist.")

coreFound :: Bool -> Core -> H.Html
coreFound justAddedMsg core = template title
					                   [H.link ! A.rel "stylesheet" ! A.type_ "text/css" ! A.href "/static/highlight.css"]
				                       ( when justAddedMsg (H.div ! A.class_ "msg" $ "Successfully added core #" >> H.toHtml (show cId))
					                     >> 
					                     htmlForCore core )
    where cId   = coreId core
          title = coreTitle core