module Html.Internal where

el :: String -> String -> String
el tag content =
  "<" <> tag <> ">" <> content <> "</" <> tag <> ">"
 
data Structure = Structure String
data Html = Html String
    deriving Show

instance Semigroup Structure where
  (<>) = append_

dummy = Structure ""

body_ = Structure . el "body" . escape
head_ = Structure . el "head" . escape
title_ = Structure . el "title" . escape
p1_ = Structure . el "p1" . escape
h1_ = Structure . el "h1" . escape
li_ = Structure . el "li" . escape

ul_ lx = foldr append_ (Structure "")  (map li_ $ map getStructureString lx)

append_ (Structure a) (Structure b) = Structure (a <> b)
render (Html str) = str

getStructureString (Structure str ) = str

html_ title content =  Html (f . head_ $ f (append_ (title_ title ) (body_ content)))
  where f = getStructureString

escape :: String -> String
escape =
  let
    escapeChar c =
      case c of
        '<' -> "&lt;"
        '>' -> "&gt;"
        '&' -> "&amp;"
        '"' -> "&quot;"
        '\'' -> "&#39;"
        _ -> [c]
  in
    concat . map escapeChar
