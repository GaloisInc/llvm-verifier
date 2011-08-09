module Text.PrettyPrint.Pretty where

import Text.PrettyPrint.HughesPJ

class Pretty a where pp :: a -> Doc

instance (Pretty a) => Pretty (Maybe a) where
  pp Nothing  = text "Nothing"
  pp (Just x) = text "Just" <+> pp x


