module TestSudokuBoard where

  import Test.HUnit
  import SudokuBoard

  sampleRows :: Rows
  sampleRows = [ [Nothing, Just 8, Nothing, Nothing, Nothing, Just 3, Nothing, Nothing, Nothing]
               , [Just 6, Just 5, Just 9, Just 2, Nothing, Nothing, Just 7, Just 3, Just 1]
               , [Nothing, Nothing, Nothing, Just 1, Nothing, Just 9, Just 4, Just 5, Nothing]
               , [Nothing, Nothing, Just 6, Just 7, Nothing, Nothing, Nothing, Nothing, Nothing]
               , [Just 3, Nothing, Just 2, Nothing, Just 1, Nothing, Just 8, Nothing, Just 5]
               , [Nothing, Nothing, Nothing, Nothing, Nothing, Just 4, Just 6, Nothing, Nothing]
               , [Nothing, Just 2, Just 4, Just 6, Nothing, Just 7, Nothing, Nothing, Nothing]
               , [Just 7, Just 9, Just 3, Nothing, Nothing, Just 2, Just 1, Just 8, Just 6]
               , [Nothing, Nothing, Nothing, Just 3, Nothing, Nothing, Nothing, Just 7, Nothing]
               ]




  sampleBoard :: Board

  SudokuBoardTests = TestList [

                              ]
