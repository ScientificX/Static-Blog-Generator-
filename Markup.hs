module Markup 
( Document,
    Structure(..)
) where

import Numeric.Natural

type Document = [Structure]

data Structure 
  = Header Natural String
  | Paragraph String
  | UnorderedList [String]
  | OrderedList [String]
  | Codeblock [String]
  deriving Show


hello_world = Paragraph "Hello, World !"

head_par :: Document
head_par = [ Header 1 "Welcome"
           , Paragraph "we begin"
           ]

parse :: String -> Document
parse = parseLines [] . lines

parseLines :: [String] -> [String] -> Document
parseLines currParsed txts = 
  let paragraph = Paragraph ( unlines $ reverse currParsed )
    in
      case txts of
        [] -> [paragraph]
        currLine:rest -> 
          if trim currLine == ""
            then 
              paragraph:parseLines [] rest
          else
            parseLines (currLine:currParsed) rest
trim = unwords . words