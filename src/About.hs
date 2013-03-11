{-# LANGUAGE OverloadedStrings #-}

module About (about) where

import Templates

import Happstack.Server

import qualified Text.Blaze.Html4.Strict as H
import qualified Text.Blaze.Html4.Strict.Attributes as A

about :: ServerPart Response
about = ok . toResponse $ template "about" 
                                   []
                                   (H.p "about...!?")