{-# LANGUAGE NoMonomorphismRestriction, OverloadedStrings #-}
module Lib where
import Control.Lens
import Data.Aeson
import Data.Aeson.Lens
import qualified Data.Text as T

jsonBlob :: T.Text
jsonBlob = "{\"foo\": \"hello\", \"bar\": [\"world\"]}"

main :: IO ()
main = do
    print $ jsonBlob ^. key "foo" . _String
    print $ jsonBlob ^. key "bar" . nth 0 . _String
