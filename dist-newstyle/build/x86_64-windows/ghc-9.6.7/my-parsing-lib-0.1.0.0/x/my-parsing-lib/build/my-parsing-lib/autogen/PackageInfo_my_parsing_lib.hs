{-# LANGUAGE NoRebindableSyntax #-}
{-# OPTIONS_GHC -fno-warn-missing-import-lists #-}
{-# OPTIONS_GHC -w #-}
module PackageInfo_my_parsing_lib (
    name,
    version,
    synopsis,
    copyright,
    homepage,
  ) where

import Data.Version (Version(..))
import Prelude

name :: String
name = "my_parsing_lib"
version :: Version
version = Version [0,1,0,0] []

synopsis :: String
synopsis = "Little parsing thing"
copyright :: String
copyright = ""
homepage :: String
homepage = ""
