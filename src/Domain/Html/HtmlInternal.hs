{-# LANGUAGE ConstrainedClassMethods #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}

module Domain.Html.HtmlInternal where

import Data.Text (Text)
import Data.Text qualified as Text
import Data.String (IsString(..))
import Data.List (sortOn)

newtype HtmlTextContent (esc :: EscapedU) a = HtmlTextContent { unHtmlTextContent :: a}
  deriving (Show)

instance Semigroup a => Semigroup (HtmlTextContent esc a) where
  (HtmlTextContent a0) <> (HtmlTextContent a1) = HtmlTextContent (a0 <> a1)

instance Monoid a => Monoid (HtmlTextContent esc a) where
  mempty = HtmlTextContent mempty 

newtype Html (esc :: EscapedU) a = Html (HtmlTextContent esc a)
  deriving (Show)

newtype Structure (esc :: EscapedU) a =  Structure {unStructure :: HtmlTextContent esc a}
  deriving (Show)

instance Semigroup a => Semigroup (Structure esc a) where
  (Structure a0) <> (Structure a1) = Structure (a0 <> a1)

instance Monoid a => Monoid (Structure esc a) where
  mempty = Structure mempty 


instance IsString a => IsString (Structure esc a) where
  fromString = Structure . HtmlTextContent . fromString

type Title a = a

data Tag = TagHtml
         | TagBody
         | TagHead
         | TagTitle
         | TagP
         | TagH1
         | TagUl
         | TagOl
         | TagLi
         | TagPre

data EscapedU = Escaped | Unescaped

class (Ord a, Monoid a, Escapable a) => WrapHtml a where
  braStart_ :: a
  braEnd_ :: a
  ket_ :: a 
  showTag :: Tag -> a
  
  append_ :: Structure 'Escaped a -> Structure 'Escaped a -> Structure 'Escaped a
  append_ = (<>)
  
  openTag_ :: Tag -> HtmlTextContent 'Escaped a
  openTag_ tag = HtmlTextContent $ braStart_ <> showTag tag <> ket_
  
  closeTag_ :: Tag -> HtmlTextContent 'Escaped a
  closeTag_ tag = HtmlTextContent $ braEnd_ <> showTag tag <> ket_
  
  el ::  Tag -> HtmlTextContent 'Escaped a -> Structure 'Escaped a
  el tag content = Structure $ openTag_ tag <> content <> closeTag_ tag

  wrapStruct :: Tag -> Structure 'Escaped a -> Structure 'Escaped a
  wrapStruct tag struct = Structure (openTag_ tag) <> struct <> Structure (closeTag_ tag)

  body_ ::  Structure 'Escaped a -> Structure 'Escaped a
  body_ = wrapStruct TagBody
  
  title_ :: a -> Structure 'Escaped a
  title_ = el TagTitle . escape
  
  head_ ::  Structure 'Escaped a -> Structure 'Escaped a
  head_ = wrapStruct TagHead
  
  p_ ::  a -> Structure 'Escaped a
  p_ = el TagP . escape
  
  h1_ ::  a -> Structure 'Escaped a
  h1_ = el TagH1 . escape

  code_ :: a -> Structure 'Escaped a
  code_ = el TagPre . escape


  li_ :: Structure 'Escaped a -> Structure 'Escaped a
  li_ = wrapStruct TagLi

  ul_ :: Foldable t => t (Structure 'Escaped a) -> Structure 'Escaped a
  ul_ = foldMap li_

  ol_ :: [Structure 'Escaped a] -> Structure 'Escaped a
  ol_ = ul_ . sortOn (unHtmlTextContent . unStructure)

  html_ ::  a -> Structure 'Escaped a -> Html 'Escaped a
  html_ title body = Html . unStructure $ head_ (title_ title) <> body_ body


instance WrapHtml Text where
  braStart_ = "<"
  braEnd_ = "</"
  ket_ = ">"
  showTag TagHtml = "html"
  showTag TagBody = "body"
  showTag TagHead = "head"
  showTag TagTitle = "title"
  showTag TagP = "p"
  showTag TagH1 = "h1"
  showTag TagUl = "ul"
  showTag TagLi = "li"
  showTag TagOl = "ol"
  showTag TagPre = "pre"

class Escapable a where
  escape :: a -> HtmlTextContent 'Escaped a

instance Escapable Text where
  escape = HtmlTextContent . Text.pack . concatMap escapeChar . Text.unpack
    where
      escapeChar c =
        case c of
          '<' -> "&lt;"
          '>' -> "&gt;"
          '&' -> "&amp;"
          '"' -> "&quot;"
          '\'' -> "&#39;"
          _ -> [c]
    
  
render :: Html esc a -> a
render (Html a) = unHtmlTextContent a