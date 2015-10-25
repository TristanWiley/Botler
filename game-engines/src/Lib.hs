{-# LANGUAGE CPP #-}
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
import Data.Maybe
import Data.Monoid
import System.IO
import qualified Data.Array as A
import qualified Data.Aeson.TH as AT
import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as L
import qualified Data.Conduit.Binary as CB
import qualified Data.Conduit.List as CL
import qualified Data.Text as T

-- Definition of a "Game"
#define deriveAllJSONs ((concat <$>) . mapM (AT.deriveJSON (AT.defaultOptions { AT.allNullaryToStringTag = False })))

newtype PlayerId = PID Int deriving (Eq, Num, Show)

data GameStatus = CurrentTurn PlayerId | Drawn | PlayerWin PlayerId deriving Show
deriveAllJSONs [''PlayerId, ''GameStatus]

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

gameLoop :: (ToJSON s, ToJSON m, FromJSON s, FromJSON m) => Game s m -> ConduitM B.ByteString B.ByteString IO ()
gameLoop g = loop (g ^. blankState) where
    emit = yield . L.toStrict . (<>"\n") . encode
    loop state = do
        liftA2 (>>=) await (pure (decode . L.fromStrict)) >>= \case
            Just (jsonLine :: Value) -> do
                pure (jsonLine ^? nth 0 . _String) `bindJust` \case
                    "getState" -> emit state >> loop state
                    "checkStatus" -> emit ((g ^. checkStatus) state) >> loop state
                    "makeMove" -> pure (jsonLine ^? nth 1 . _JSON) `bindJust` \(move :: m) ->
                        pure ((g ^. makeMove) move state) >>= maybe (loop state) loop
                    "validMoves" -> emit ((g ^. validMoves) state) >> loop state
                    _ -> loop state
            Nothing -> loop state

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

deriveAllJSONs [''RPSMove, ''RPSState]

-- 2-player Tron
data TronMove = KeepGoing | TurnCW | TurnCCW deriving Show
data TronDirection = North | East | South | West deriving (Enum, Show)
type TronCoord = (Int, Int)
type TronPlayer = (TronCoord, TronDirection)
type TronGrid = A.Array TronCoord (Maybe PlayerId)
data TronState = TS TronPlayer TronPlayer TronGrid deriving Show

applyTurn :: TronMove -> TronDirection -> TronDirection
applyTurn dir x = toEnum ((fromEnum x + f dir) `mod` 4) where
        f KeepGoing = 0; f TurnCW = 1; f TurnCCW = -1

getDelta North = ( 0,-1)
getDelta South = ( 0, 1)
getDelta West  = (-1, 0)
getDelta East  = ( 1, 0)

updatePosition (x,y) dir = let (dx, dy) = getDelta dir in (x+dx, y+dy)

tronCheckStatus (TS (p1,d1) (p2,d2) board) = let
    [alive1, alive2] = map (isNothing . (board A.!)) [p1,p2]
    in case (alive1, alive2) of
        (True,True) -> CurrentTurn 0
        (True,False) -> PlayerWin 0
        (False,True) -> PlayerWin 1
        (False,False) -> Drawn

tronBikeGame :: TronCoord -> Game TronState (TronMove, TronMove)
tronBikeGame size@(width, height) = Game {
    _blankState = TS ((0,0), South) (size, North) (A.listArray ((0,0),size) (repeat Nothing)),
    _checkStatus = tronCheckStatus,
    _makeMove = \(m1,m2) state@(TS (p1,d1) (p2,d2) board) -> case tronCheckStatus state of
        CurrentTurn _ -> let
            [d1', d2'] = zipWith applyTurn [m1,m2] [d1,d2]
            [p1', p2'] = zipWith updatePosition [p1, p2] [d1', d2']
            in Just (TS (p1',d1') (p2', d2') (board A.// [(p1,Just 0), (p2,Just 1)]))
        _ -> Nothing,
    _renderState = T.pack . show, -- TODO: SVG
    _validMoves = \state -> case tronCheckStatus state of
        CurrentTurn _ -> (liftA2 (,) <*> id) [KeepGoing, TurnCW, TurnCCW]
        _ -> []
    }

-- 2-player Tic-Tac-Toe
data TTTMove = A0 | A1 | A2|
               B0 | B1 | B2|
               C0 | C1 | C2 deriving (Eq, Show)
{-data TTTState = A0taken | A1taken | A2taken |
                B0taken | B1taken | B3taken |
                C0taken | C1taken | C3taken deriving Show
-}
-- JSON hello world

{-
jsonBlob :: T.Text
jsonBlob = "{\"foo\": \"hello\", \"bar\": [\"world\"]}"

main :: IO ()
main = do
    print $ jsonBlob ^. key "foo" . _String
    print $ jsonBlob ^. key "bar" . nth 0 . _String
-}
