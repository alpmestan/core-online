{-# LANGUAGE OverloadedStrings #-}

module Templates.CoreForm where

import Templates

import Data.Text (Text)
import Text.Blaze ((!), toValue)
import qualified Text.Blaze.Html4.Strict as H
import qualified Text.Blaze.Html4.Strict.Attributes as A

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