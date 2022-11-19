{-# LANGUAGE ConstrainedClassMethods #-}

module Domain.Html.Html (Html, Title, Structure, html_, p_, h1_, append_, render) where

import Data.Text (Text)
import Data.Text qualified as Text
import Data.String (IsString(..))

newtype Html a = Html a
  deriving (Show)

instance Semigroup a => Semigroup (Html a) where
  (Html a0) <> (Html a1) = Html (a0 <> a1)

instance IsString a => IsString (Html a) where
  fromString = Html . fromString

newtype Structure a = Structure {unStructure :: a}
  deriving (Show)

instance Semigroup a => Semigroup (Structure a) where
  (Structure a0) <> (Structure a1) = Structure (a0 <> a1)

instance IsString a => IsString (Structure a) where
  fromString = Structure . fromString

type Title a = a

data Tag = TagHtml
         | TagBody
         | TagHead
         | TagTitle
         | TagP
         | TagH1


class Semigroup a => WrapHtml a where
  braStart_ :: a
  braEnd_ :: a
  ket_ :: a 
  showTag :: Tag -> a
  
  append_ :: Structure a -> Structure a -> Structure a
  append_ = (<>)
  
  openTag_ :: Tag -> a
  openTag_ tag = braStart_ <> showTag tag <> ket_
  
  closeTag_ :: Tag -> a
  closeTag_ tag = braEnd_ <> showTag tag <> ket_
  
  el ::  Tag -> a -> Structure a
  el tag content = Structure $ openTag_ tag <> content <> closeTag_ tag

  wrapStruct :: Tag -> Structure a -> Structure a
  wrapStruct tag struct = Structure (openTag_ tag) <> struct <> Structure (closeTag_ tag)

  body_ ::  Structure a -> Structure a
  body_ = wrapStruct TagBody
  
  title_ :: a -> Structure a
  title_ = el TagTitle
  
  head_ ::  Structure a -> Structure a
  head_ = wrapStruct TagHead
  
  p_ ::  a -> Structure a
  p_ = el TagP
  
  h1_ ::  a -> Structure a
  h1_ = el TagH1
  
  html_ ::  a -> Structure a -> Html a
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

class Escapable a where
  escape :: a -> a

instance Escapable Text where
  escape = Text.pack . concatMap escapeChar . Text.unpack
    where
      escapeChar c =
        case c of
          '<' -> "&lt;"
          '>' -> "&gt;"
          '&' -> "&amp;"
          '"' -> "&quot;"
          '\'' -> "&#39;"
          _ -> [c]
    
  
render :: Html a -> a
render (Html a) = a