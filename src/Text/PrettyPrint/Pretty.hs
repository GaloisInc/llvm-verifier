module Text.PrettyPrint.Pretty where

import Text.PrettyPrint.HughesPJ

class Pretty a where pp :: a -> Doc
