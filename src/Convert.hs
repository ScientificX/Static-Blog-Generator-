{-# OPTIONS_GHC -Wno-incomplete-patterns #-}
module
Convert(

) where

import qualified Html.Internal as H
import qualified Markup as Mk
import Html (h1_)
import qualified GHC.Exts.Heap as H

-- we want to conveert from type Markup.Structure to type Html.Structure

convert :: Mk.Document  -- ^ 
  -> [H.Structure]
convert = map conv
    where
        conv thing =
            case thing of
                Mk.Header nat str -> H.h1_ str
                Mk.Paragraph str -> H.p1_ str
                Mk.UnorderedList list ->  H.ul_ $ map H.li_ list
                Mk.OrderedList list ->  H.ol_ $ map H.li_ list
                Mk.Codeblock list -> H.code_ (unlines list)

