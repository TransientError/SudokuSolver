import TestSudokuBoard
import Test.HUnit

main :: IO Counts
main = runTestTT TestSudokuBoard.sudokuBoardTests
