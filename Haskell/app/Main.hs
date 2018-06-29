module Main where

import Data.Char (toUpper, isLetter)
import System.Console.ANSI
import System.IO (BufferMode(NoBuffering), hSetBuffering, stdout)
import System.IO.HiddenChar (getHiddenChar)

import Hangman


main :: IO ()
main = do
  hSetBuffering stdout NoBuffering
  hideCursor

  secretWord <- readSecretWord
  runGame (Hangman.init secretWord 3)


runGame :: Game -> IO ()
runGame game = do
  showCaption
  displayGame
  case Hangman.state game of
    Hangman.Running ->
      nextGuess >>= runGame
    Hangman.Won ->
      displayWon
    Hangman.Lost ->
      displayLost
  where
    displayGame = do
      putStrLn $ Hangman.display game
      putStrLn "\n\n"
      putStrLn $ "you have " ++ show (Hangman.lifes game) ++ " lifes left"
    displayWon = do
      setSGR [SetColor Foreground Vivid Green]
      putStrLn "\ncongrats - you WON"
    displayLost = do
      setSGR [SetColor Foreground Vivid Red]
      putStrLn "\nsorry - you LOST"
    nextGuess = do
      l <- readLetter
      return $ Hangman.guess l game
    readLetter = do
      c <- readChar
      if isLetter c then
        return c
      else
        readLetter


showCaption :: IO ()
showCaption = do
  clearScreen
  setSGR [SetColor Foreground Vivid Blue]
  putStrLn ""
  putStrLn "-------------------"
  putStrLn "-  H A N G M A N  -"
  putStrLn "-------------------"
  putStrLn "\n\n"
  setSGR [Reset]


readSecretWord :: IO String
readSecretWord = do
  showCaption

  setSGR [SetColor Foreground Vivid Green]
  putStr "please input your secret word: "
  setSGR [SetColor Foreground Vivid Red]
  res <- unfoldIO readSecretChar
  setSGR [Reset]
  return res

  where
    unfoldIO m = do
      o <- m
      case o of
        Nothing -> return []
        Just c  -> (c:) <$> unfoldIO m
    readSecretChar = do
      inpC <- readChar
      case inpC of
        c | isLetter c -> do
          putChar '*'
          return $ Just $ toUpper c
        _ -> do
          putChar '\n'
          return Nothing


readChar :: IO Char
readChar = getHiddenChar