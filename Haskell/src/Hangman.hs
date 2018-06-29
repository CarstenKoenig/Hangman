module Hangman 
    ( Game
    , State (..)
    , Hangman.init
    , state
    , lifes
    , display
    , guess
    ) where

import Data.Char (toUpper)
import Data.List (intersperse)

data Game = Game
    { cells :: [Cell] 
    , lifes :: Int
    }


data Cell
    = Hidden Char
    | Revealed Char
    deriving Eq


data State
    = Running
    | Won
    | Lost
    deriving (Eq, Show)


init :: String -> Int -> Game
init word = 
    Game (map Hidden word)


state :: Game -> State
state game
    | allRevealed = Won
    | noLifesLeft = Lost
    | otherwise   = Running
    where
        allRevealed = all isRevealed $ cells game
        noLifesLeft = lifes game == 0
        isRevealed (Revealed _) = True
        isRevealed (Hidden _)   = False


display :: Game -> String
display game =
    intersperse ' ' $ map displayCell $ cells game
    where
        displayCell (Hidden _) = '_'
        displayCell (Revealed c) = c


guess :: Char -> Game -> Game
guess c game
    | state game == Running = 
        game { cells = revealed
             , lifes = lifes'
             }
    | otherwise = game
    where
        lifes'   = if revealed == cells game
                    then lifes game - 1
                    else lifes game
        revealed = map reveal $ cells game
        reveal cell@(Hidden c')
            | toUpper c == c'   = Revealed c'
            | otherwise         = cell
        reveal cell             = cell
