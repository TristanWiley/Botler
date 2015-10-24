{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
module Lib where
import Control.Lens
import Data.Aeson
import Data.Aeson.Lens
import qualified Data.Text as T

-- Definition of a "Game"

data GameStatus = StillGoing | Drawn | PlayerWinId Int

data Game state move = Game {
    _blankState :: state,
    _checkStatus :: state -> GameStatus,
    _makeMove :: move -> state -> Maybe state,
    _renderState :: state -> T.Text, -- output is SVG
    _validMoves :: state -> [move]
    }

makeLenses ''Game

-- 2-player Rock-paper-scissors
data RPSMove = Rock | Paper | Scissors deriving (Eq, Show)
data RPSState = ZeroProvided | OneProvided RPSMove | TwoProvided RPSMove RPSMove deriving Show

rpsCompare Rock Paper = LT
rpsCompare Paper Scissors = LT
rpsCompare Scissors Rock = LT
rpsCompare x y | x == y = EQ
rpsCompare _ _ = GT

rockPaperScissors :: Game RPSState RPSMove
rockPaperScissors = Game {
    _blankState = ZeroProvided,
    _checkStatus = \case
        ZeroProvided -> StillGoing
        OneProvided _ -> StillGoing
        TwoProvided l r -> case l `rpsCompare` r of
            LT -> PlayerWinId 1
            EQ -> Drawn
            GT -> PlayerWinId 0,
    _makeMove = \m -> \case
        ZeroProvided -> Just $ OneProvided m
        OneProvided m1 -> Just $ TwoProvided m1 m
        TwoProvided _ _ -> Nothing,
    _renderState = T.pack . show, -- TODO: SVG
    _validMoves = const [Rock, Paper, Scissors]
    }

-- JSON hello world

jsonBlob :: T.Text
jsonBlob = "{\"foo\": \"hello\", \"bar\": [\"world\"]}"

main :: IO ()
main = do
    print $ jsonBlob ^. key "foo" . _String
    print $ jsonBlob ^. key "bar" . nth 0 . _String
