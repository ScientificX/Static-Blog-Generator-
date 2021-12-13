module Markup 
(
    Document,
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

