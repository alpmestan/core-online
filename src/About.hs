{-# LANGUAGE OverloadedStrings #-}

module About (about) where

import Templates.About

import Happstack.Server

about :: ServerPart Response
about = ok . toResponse $ aboutTpl