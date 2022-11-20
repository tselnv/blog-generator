module Domain.Markup.MarkupText where

import Domain.Markup.Markup
import Data.Text (Text)
import Data.Text qualified as Text

newtype MarkupText = MarkupText {unMarkupText:: Text}
    deriving (Eq)

instance Parser MarkupText where
  parse = parseLines [] . map MarkupText . Text.lines . unMarkupText
    where
        parseLines :: [MarkupText] -> [MarkupText] -> [Structure MarkupText]
        parseLines currentParagraph txts = 
            let
                paragraph = Paragraph $ MarkupText (Text.unlines $ map unMarkupText (reverse currentParagraph))
            in
                case txts of
                [] -> [paragraph]
                currentLine : rest ->
                    if trim currentLine == MarkupText ""
                    then
                        paragraph : parseLines [] rest
                    else
                        parseLines (currentLine : currentParagraph) rest

        trim :: MarkupText -> MarkupText
        trim = MarkupText . Text.unwords . Text.words . unMarkupText