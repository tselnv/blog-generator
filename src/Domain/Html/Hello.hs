{-# LANGUAGE DataKinds #-}
module Domain.Html.Hello (main) where

import Domain.Html.Html

import Data.Text (Text)
import Data.Text qualified as Text
import qualified Data.Text.IO as Text
import Data.Time (getCurrentTime)

main :: IO ()
main = do
    time <- show <$> getCurrentTime 
    let html = render $ myhtml time
    putStrLn (Text.unpack html)
    Text.writeFile ".hello.html" html

myhtml :: String -> Html Escaped Text
myhtml time =
  html_
    "My title"
    ( append_
      (h1_ "Heading")
      ( 
          p_ "Paragraph #1" <>
          p_ "Paragraph #2" <>
          ul_
            [ p_ "item 1"
            , p_ "item 2"
            , p_ "item 3"
            ] <>
          p_ (Text.pack time)
      )
    )
