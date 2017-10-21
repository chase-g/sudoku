import scala.swing._
import java.awt.Dimension
import java.awt.Color._

class UI extends MainFrame {
  val t0 = System.nanoTime()
import scala.util.Random
//randomly shuffle array of 1-9 or A-? to form first row
val widthIndex = 8
val width = widthIndex + 1
val all = (1 to 9).toList
val firstRow = List(Random.shuffle(1 to width).toList)
//val test = List(List(1,2,3,4,5,6,7,8,9))

def getColumn(index: Int, puzzle: List[List[Int]]): List[Int] = { 
    val column = for(outer <- puzzle) yield {
      outer(index)
    }
    column
  }
//Use to create list of existing 3 x 3 group values
def getGroup(indexAcross: Int, indexDown: Int,  puzzle: List[List[Int]]): List[Int] = {
  val xStart = indexDown / 3 * 3
  val yStart = indexAcross / 3 * 3
  puzzle.slice(xStart, xStart + 3).map(row => row.slice(yStart, yStart + 3)).flatten
}

//Use to check potential new value against existing column values and then produce unique value
def nextValue(available: List[Int], indexAcross: Int, indexDown: Int,  puzzle: List[List[Int]]): Int = {
  if(!available.isEmpty){
  val nextItem = Random.shuffle(available)
  val column = getColumn(indexAcross, puzzle) //change from indexDown
  val group = getGroup(indexAcross, indexDown, puzzle)
  //println("Available " + available)
  //println("Column: " + column)
 // println("Group: " + group)
 // println("Considering " + nextItem(0))
    if (!column.contains(nextItem(0)) && !group.contains(nextItem(0))) {
     // println("Using: " + nextItem(0))
      return nextItem(0)
    } else if(nextItem.length == 0) {
      return -1
    }
      else {
      nextValue(nextItem.drop(1), indexAcross, indexDown, puzzle)
  }
 } else return -1
}


def createRow(currentRow: List[Int], puzzle: List[List[Int]], counter: Int): List[Int] = {
  //println("Counter: " + counter)
  if(currentRow.length != width){
    val avail = all.filter(x => !currentRow.contains(x)) //set available values
 //nextValue(available, across, down, puzzle)
    val nextNum = nextValue(avail, currentRow.length, puzzle.length, puzzle) //get nextValue with available values, across at current puzzle index length, down at index, on puzzle arg
 //changed from puzzle.length - 1
    if(nextNum > 0){ //If nextNum is positive 
   // println("Plan A")
   // println("currentRow" + currentRow)
   // println("avail: " + avail)
   // println("nextNum: " + nextNum)
      val backtrack = counter + 1
      val runningRow = currentRow :+ nextNum//append it to curentRow
      //println("Row: " + runningRow)
      createRow(runningRow, puzzle, backtrack) //call createRow recursively with value added to row
    } else { //otherwise backtrack to prior value when calling createRow recursively
    //  println("Plan B")
      if(counter > 0){
      val runningRow = currentRow.slice(0, currentRow.length - (counter / 2))
      createRow(runningRow, puzzle, 0)
      } else {
        createRow(List(), puzzle, 0)
      }
    }
  } else return currentRow
}

/*createRow(List(), test, 0)
val t1 = System.nanoTime() 
println("Elapsed time: " + (t1 - t0) + " nanoseconds")
*/


def createPuzzle(sudoku: List[List[Int]]): List[List[String]] = {
 // println(sudoku)
  if(sudoku.length == width) return sudoku.map(_.map(_.toString))
  else {
    val nextRow = createRow(List(), sudoku, 0)
    val runningPuzzle = sudoku :+ nextRow
    createPuzzle(runningPuzzle)
  }
}

def createGaps(currentPuzzle: List[List[String]], indexAcross: Int = 0, indexDown: Int = 0): List[List[String]] = {
  val r = scala.util.Random
  val choice = r.nextInt(9)
  if(indexDown == widthIndex && indexAcross == widthIndex) return currentPuzzle
  else if (choice > 5){
    val gappedRow = currentPuzzle(indexDown).updated(indexAcross, " ")
    val gappedPuzzle = currentPuzzle.updated(indexDown, gappedRow)
    if(indexAcross != widthIndex) createGaps(gappedPuzzle, indexAcross + 1, indexDown)
    else createGaps(gappedPuzzle, 0, indexDown + 1)
    } 
  else{
          if(indexAcross != widthIndex) createGaps(currentPuzzle, indexAcross + 1, indexDown)
          else createGaps(currentPuzzle, 0, indexDown + 1)
  }
}

def printPuzzle(puzzle: List[List[String]]): Unit = {
  //val puzzle = createPuzzle(start)
  println("-----" * 9)
  for(i <- 0 until puzzle.length; n <- 0 until puzzle(0).length){
    if(n == puzzle.length - 1 && (i == 2 || i == 5)){
      print(" | ")     
      print(puzzle(i)(n))
      print(" | ")
      println("")
      println("-----" * 9)
      println("-----" * 9)
    }
    else if(n == widthIndex){
      print(" | ")
      print(puzzle(i)(n))
      print(" | ")
      println("")
      println("-----" * 9)
    }
    else if(n == 3 || n == 6) {
      print(" || ")
      print(puzzle(i)(n))
    }
    else{
    print(" | ")
    print(puzzle(i)(n))
    print("")
    }
  }
}

val solution = createPuzzle(firstRow)
val sudoku = createGaps(solution)
printPuzzle(sudoku) 

  def restrictHeight(s: Component) {
    s.maximumSize = new Dimension(Short.MaxValue, s.preferredSize.height)
  }
  title = "Sudoku"
  
  val buttonCalc = new Button("Calculate")
  val btnSize = new Dimension(50, 50)
  
contents = new BoxPanel(Orientation.Vertical) {
      for(i <- 0 until sudoku.length) {
            contents += new BoxPanel(Orientation.Horizontal){
            for(n <- 0 until sudoku(i).length) {
              contents += new BoxPanel(Orientation.Horizontal){
              contents += new Button(sudoku(i)(n)) {
                 minimumSize = btnSize
                 maximumSize = btnSize
                 preferredSize = btnSize
                 if((n > 2 && n < 6 && i < 3) || (n > 2 && n < 6 && i > 5)){
                 foreground = java.awt.Color.green
                 } else if((i > 2 && i < 6 &&  n < 3) || (i > 2 && i < 6 && n > 5)) {
                   foreground = java.awt.Color.orange
                 }
                 else {
                   foreground = java.awt.Color.black
                 }
              }

            }
          } 
      }
    }
   
    for (e <- contents)
      e.xLayoutAlignment = 0.0
      border = Swing.EmptyBorder(20, 20, 20, 20)
  }

  /*listenTo(buttonCalc)
  
  reactions += {/*
    case ButtonClicked(buttonCalc) => {
      
    }*/
  }*/
}


object swingSudoku {
  def main(args: Array[String]) {
    val ui = new UI
    ui.visible = true
  }
}
