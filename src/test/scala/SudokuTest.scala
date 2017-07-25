import org.scalatest.FunSuite

class SudokuTest extends FunSuite {

  val grid = Vector(Vector(0,0,4,8,0,0,0,1,7), Vector(6,7,0,9,0,0,0,0,0), Vector(5,0,8,0,3,0,0,0,4),
    Vector(3,0,0,7,4,0,1,0,0), Vector(0,6,9,0,0,0,7,8,0), Vector(0,0,1,0,6,9,0,0,5),
    Vector(1,0,0,0,8,0,3,0,6), Vector(0,0,0,0,0,6,0,9,1), Vector(2,4,0,0,0,1,5,0,0))


  val sudokuQuestion = new Sudoku

  test("Testing for sudoku where solution exists")
  {
    assert(sudokuQuestion.computeSudoku(grid))
  }

  val invalidGrid = Vector(Vector(1,0,0,9,0,0,0,1,6), Vector(0,0,0,2,0,0,6,0,1), Vector(0,0,1,0,9,0,0,0,4),
    Vector(3,0,0,7,4,0,1,0,0), Vector(0,0,9,0,0,7,7,8,0), Vector(5,0,1,0,6,9,0,0,5),
    Vector(1,0,0,0,8,0,3,0,6), Vector(0,0,0,0,0,6,0,9,1), Vector(2,4,0,0,0,1,5,0,0))

  test("Invalid sudoku ")
  {
    assert(sudokuQuestion.computeSudoku(invalidGrid))
  }


}