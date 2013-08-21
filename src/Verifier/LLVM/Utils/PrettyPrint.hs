module Verifier.LLVM.Utils.PrettyPrint
  ( BitWidth
  , commaSepList
  , ppIntType
  , ppPtrType
  , ppArrayType
  , ppVectorType
  , ppIntVector
  ) where

import Text.PrettyPrint.Leijen

-- | Print list of documents separated by commas and spaces.
commaSepList :: [Doc] -> Doc
commaSepList l = hcat (punctuate (comma <> char ' ') l)

type BitWidth = Int 

-- | Pretty print int type with width.
ppIntType :: BitWidth -> Doc
ppIntType i = char 'i' <> integer (toInteger i)

-- | Pretty print pointer type.
ppPtrType :: Doc -> Doc
ppPtrType tp = tp <> char '*'

ppArrayType :: Int -> Doc -> Doc
ppArrayType n e = brackets (int n <+> char 'x' <+> e)

ppVectorType :: Int -> Doc -> Doc
ppVectorType n e = angles (int n <+> char 'x' <+> e)

ppIntVector :: Int -> BitWidth -> Doc
ppIntVector n w = ppVectorType n (ppIntType w)
