module Domain.Html.HtmlTextInternal where

import Data.Text (Text)
import Data.Text qualified as Text
import Data.String (IsString(..))

import Domain.Html.HtmlInternal
    ( Escapable(..),
      EscapedContent(EscapedContent),
      Tag(TagPre, TagHtml, TagBody, TagHead, TagTitle, TagP, TagH1,
          TagUl, TagLi, TagOl),
      WrapHtml(braStart_, showTag, ket_, braEnd_) )

newtype TextHtml = TextHtml { unTextHtml :: Text }
  deriving (Eq, Ord)

instance Semigroup TextHtml where
  (TextHtml a) <> (TextHtml b) = TextHtml (a <> b)

instance Monoid TextHtml where
  mempty = TextHtml Text.empty 

instance IsString TextHtml where
  fromString = TextHtml . fromString

instance WrapHtml TextHtml where
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


instance Escapable TextHtml where
  escape = EscapedContent . TextHtml . Text.pack . concatMap escapeChar . Text.unpack . unTextHtml
    where
      escapeChar c =
        case c of
          '<' -> "&lt;"
          '>' -> "&gt;"
          '&' -> "&amp;"
          '"' -> "&quot;"
          '\'' -> "&#39;"
          _ -> [c]
    
