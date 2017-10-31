module Main where

import qualified SudokuBoard as S
import qualified Data.Maybe as M

main :: IO ()
main = do
  input <- readFile "input.txt"
  let rows = lines input
      board = map words rows
      sudokuBoardRows = map (map convertToMaybe) board
      solved = solve (S.initializeBoardFromRows sudokuBoardRows)
      returnMaybe = map (map (show . M.fromJust)) (S.rows solved)
  writeFile "output.txt" (unlines (map unwords returnMaybe))

convertToMaybe :: String -> Maybe Integer
convertToMaybe "0" = Nothing
convertToMaybe n = Just (read n)

solve :: S.Board -> S.Board
solve board = solve' [board]
  where solve' [] = error "not solveable"
        solve' (board:rest) = if S.isFull board then board
          else solve' (validBoards ++ rest)
            where newBoards = map (S.insert board (S.findNextEmpty board)) [1..9]
                  validBoards = filter S.validateBoard newBoards
