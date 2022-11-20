module Domain.Markup.MarkupString where

import Domain.Markup.Markup

newtype MarkupString = MarkupString {unMarkupString :: String}
    deriving (Eq)

instance Parser MarkupString where
  parse = parseLines [] . map MarkupString . lines . unMarkupString
    where
        parseLines :: [MarkupString] -> [MarkupString] -> [Structure MarkupString]
        parseLines currentParagraph txts =
            let paragraf = (Paragraph . MarkupString) (unlines . reverse . map unMarkupString $ currentParagraph)
            in case txts of
                    [] -> [paragraf]
                    currentLine : rest -> 
                        if nullAfterTrim currentLine
                            then paragraf : parseLines [] rest
                            else parseLines (currentLine : currentParagraph) rest

        nullAfterTrim :: MarkupString -> Bool
        nullAfterTrim = null . unwords . words . unMarkupString
