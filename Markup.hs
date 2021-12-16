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
parse = parseLines Nothing . lines

parseLines :: Maybe Structure -> [String] -> Document
parseLines ctx txts = 
      case txts of
        [] -> maybeToList ctx
        currLine:rest -> 
          if trim currLine == ""
            then 
              maybe id (:) ctx $ parseLines Nothing rest
          else
            case ctx of
              Just (Paragraph x) -> parseLines (Just (Paragraph (x <> currLine))) rest
              _                  -> maybe id (:) ctx $ parseLines (Just (Paragraph currLine)) rest

trim = unwords . words
maybeToList Nothing = []
maybeToList (Just x) = [x]