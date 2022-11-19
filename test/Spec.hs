{-# LANGUAGE ImportQualifiedPost #-}

import Hello qualified

main :: IO ()
main = Hello.main
