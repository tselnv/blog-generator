module Domain.Markup.Markup (Document, Structure (..), Parser (..)) where

import Numeric.Natural (Natural)

type Document a = [Structure a]

data Structure a
  = Heading Natural a
  | Paragraph a
  | UnorderedList [a]
  | OrderedList [a]
  | CodeBlock [a]


class Parser a where
  parse :: a -> Document a



