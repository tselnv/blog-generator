module Domain.Html.HtmlInternal where

import Data.List (sortOn)

newtype EscapedContent a = EscapedContent {unEscapedContent :: a}
  deriving (Show)

newtype UnEscapedContent a = UnEscapedContent {unUnEscapedContent :: a}
  deriving (Show)

instance Semigroup a => Semigroup (EscapedContent a) where
  (EscapedContent a0) <> (EscapedContent a1) = EscapedContent (a0 <> a1)

instance Monoid a => Monoid (EscapedContent a) where
  mempty = EscapedContent mempty

newtype Html a = Html (EscapedContent a)
  deriving (Show)

newtype Structure a = Structure {unStructure :: EscapedContent a}
  deriving (Show)

instance Semigroup a => Semigroup (Structure a) where
  (Structure a0) <> (Structure a1) = Structure (a0 <> a1)

instance Monoid a => Monoid (Structure a) where
  mempty = Structure mempty

type Title a = a

data Tag
  = TagHtml
  | TagBody
  | TagHead
  | TagTitle
  | TagP
  | TagH1
  | TagUl
  | TagOl
  | TagLi
  | TagPre

class (Ord a, Monoid a, Escapable a) => WrapHtml a where
  braStart_ :: a
  braEnd_ :: a
  ket_ :: a
  showTag :: Tag -> a

  append_ :: Structure a -> Structure a -> Structure a
  append_ = (<>)

  openTag_ :: Tag -> EscapedContent a
  openTag_ tag = EscapedContent $ braStart_ <> showTag tag <> ket_

  closeTag_ :: Tag -> EscapedContent a
  closeTag_ tag = EscapedContent $ braEnd_ <> showTag tag <> ket_

  el :: Tag -> EscapedContent a -> Structure a
  el tag content = Structure $ openTag_ tag <> content <> closeTag_ tag

  wrapStruct :: Tag -> Structure a -> Structure a
  wrapStruct tag struct = Structure (openTag_ tag) <> struct <> Structure (closeTag_ tag)

  body_ :: Structure a -> Structure a
  body_ = wrapStruct TagBody

  title_ :: a -> Structure a
  title_ = el TagTitle . escape

  head_ :: Structure a -> Structure a
  head_ = wrapStruct TagHead

  p_ :: a -> Structure a
  p_ = el TagP . escape

  h1_ :: a -> Structure a
  h1_ = el TagH1 . escape

  code_ :: a -> Structure a
  code_ = el TagPre . escape

  li_ :: Structure a -> Structure a
  li_ = wrapStruct TagLi

  ul_ :: Foldable t => t (Structure a) -> Structure a
  ul_ = foldMap li_

  ol_ :: [Structure a] -> Structure a
  ol_ = ul_ . sortOn (unEscapedContent . unStructure)

  html_ :: a -> Structure a -> Html a
  html_ title body = Html . unStructure $ head_ (title_ title) <> body_ body

class Escapable a where
  escape :: a -> EscapedContent a

render :: Html a -> a
render (Html a) = unEscapedContent a
