{-# LANGUAGE OverloadedStrings #-}

module Templates.Home where

import Templates
import Templates.CoreForm

import Data.Int
import Text.Blaze ((!), toValue)
import qualified Text.Blaze.Html4.Strict as H
import qualified Text.Blaze.Html4.Strict.Attributes as A
            
homeHtml :: Int64 -> H.Html
homeHtml nbCores = template "bonsouare"
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