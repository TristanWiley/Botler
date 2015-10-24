{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
module Lib where
import Control.Applicative
import Control.Lens
import Control.Monad
import Control.Monad.Trans
import Data.Aeson
import Data.Aeson.Lens
import Data.Char
import Data.Conduit
import System.IO
import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as L
import qualified Data.Conduit.Binary as CB
import qualified Data.Conduit.List as CL
import qualified Data.Text as T

-- Definition of a "Game"

newtype PlayerId = PID Int deriving (Eq, Num, Show)

data GameStatus = CurrentTurn PlayerId | Drawn | PlayerWin PlayerId deriving Show

data Game state move = Game {
    _blankState :: state,
    _checkStatus :: state -> GameStatus,
    _makeMove :: move -> state -> Maybe state,
    _renderState :: state -> T.Text, -- output is SVG
    _validMoves :: state -> [move]
    }

makeLenses ''Game

-- generic game loop
infixl 1 `bindJust`
f `bindJust` g = f >>= maybe (return ()) g

inspectConduit = await `bindJust` \x -> liftIO (print x) >> yield x

runByteStringConduit :: Handle -> Handle -> ConduitM B.ByteString B.ByteString IO () -> IO ()
runByteStringConduit i o c = runConduit $
    CB.sourceHandle i =$=
    CB.lines =$=
    c =$=
    CB.sinkHandle o

gameLoop :: Game s m -> ConduitM B.ByteString B.ByteString IO ()
gameLoop g = loop (g ^. blankState) where
    loop state = do
        liftA2 (>>=) await (pure (decode . L.fromStrict)) `bindJust` \(jsonLine :: Value) -> do
            flip (maybe (loop state)) (jsonLine ^? nth 0 . _String) $ \functionName -> do
                liftIO $ print functionName -- TODO: switch on function goes here
        loop state

main = runByteStringConduit stdin stdout (gameLoop rockPaperScissors)

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
        ZeroProvided -> CurrentTurn 0
        OneProvided _ -> CurrentTurn 1
        TwoProvided l r -> case l `rpsCompare` r of
            LT -> PlayerWin 1
            EQ -> Drawn
            GT -> PlayerWin 0,
    _makeMove = \m -> \case
        ZeroProvided -> Just $ OneProvided m
        OneProvided m1 -> Just $ TwoProvided m1 m
        TwoProvided _ _ -> Nothing,
    _renderState = T.pack . show, -- TODO: SVG
    _validMoves = \case
        TwoProvided _ _ -> []
        _ -> [Rock, Paper, Scissors]
    }

-- JSON hello world

{-
jsonBlob :: T.Text
jsonBlob = "{\"foo\": \"hello\", \"bar\": [\"world\"]}"

main :: IO ()
main = do
    print $ jsonBlob ^. key "foo" . _String
    print $ jsonBlob ^. key "bar" . nth 0 . _String
-}
