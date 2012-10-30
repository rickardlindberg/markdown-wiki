module Types where

import qualified Data.Map as M
import Text.Pandoc

type Pages = M.Map WikiName Pandoc

type WikiName = String
