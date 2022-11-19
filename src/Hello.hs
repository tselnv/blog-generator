{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ImportQualifiedPost #-}

module Hello where

import Html

import Data.Text (Text)
import Data.Text qualified as Text
import qualified Data.Text.IO as Text

main :: IO ()
main = do
    let html = render myhtml
    putStrLn (Text.unpack html)
    Text.writeFile "html.html" html

myhtml :: Html Text
myhtml =
  html_
    "My title"
    ( append_
      (h1_ "Heading")
      ( append_
        (p_ "Paragraph #1")
        (p_ "Paragraph #2")
      )
    )
