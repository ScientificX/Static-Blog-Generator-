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

head_par = [ Header 1 "Welcome"
           , Paragraph "we begin"
           ]

parse :: String -> Document
parse = parseLines Nothing . lines

parseLines :: Maybe Structure -> [String] -> Document
parseLines ctx txts = 
      case txts of
        [] -> maybeToList ctx

        -- Header case

        ('*':' ':line):rest -> 
          maybe id (:) ctx $ ((Header 1 (trim line)):(parseLines Nothing rest))

        -- Unordered Lists

        ('-':' ':item):rest -> 
          case ctx of 
            (Just(UnorderedList list)) -> 
              parseLines (Just (UnorderedList (list <> [trim item])) ) rest
            _ ->
              maybe id (:) ctx $ parseLines (Just (Paragraph item) ) rest

        -- OrderedLists
        ('#':' ':line):rest -> 
          case ctx of
            (Just (OrderedList list)) -> 
              parseLines (Just (OrderedList (list <> [trim line]))) rest
            _ -> 
              maybe id (:) ctx $ parseLines Nothing rest
        
        -- CodeBlock
        ('>':' ':line):rest ->
          case ctx of
            (Just (Codeblock lines)) ->
              parseLines (Just (Codeblock (lines <> [trim line]))) rest
            _ ->
              maybe id (:) ctx $ parseLines Nothing rest



        -- Paragraphs           

        (line:rest) ->
          let trimLine = (trim line)
            in
              if line == ""
                then
                  maybe id (:) ctx $ (parseLines Nothing rest)
              else
                case ctx of
                  (Just (Paragraph content)) -> 
                    parseLines (Just (Paragraph (content <> (trim line)))) rest
                  _ ->
                    maybe id (:) ctx $ (parseLines (Just (Paragraph line)) rest) 


trim = unwords . words
maybeToList Nothing = []
maybeToList (Just x) = [x]