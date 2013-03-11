{-# LANGUAGE OverloadedStrings #-}

module Templates.About where

import Templates

import Text.Blaze ((!))
import qualified Text.Blaze.Html4.Strict as H
import qualified Text.Blaze.Html4.Strict.Attributes as A

aboutTpl :: H.Html
aboutTpl = template "about" 
                    []
                    (H.p ! A.class_ "about" $ do
                        "This application uses "
                        >> 
                        (H.a ! A.href "http://happstack.com/" $ "happstack")
                        >>
                        " and "
                        >>
                        (H.a ! A.href "http://github.com/alpmestan/core-online" $ "is open source")
                        >>
                        ". Feel free to submit issues and pull requests!")