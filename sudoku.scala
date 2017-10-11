
val t0 = System.nanoTime()
import scala.util.Random
//randomly shuffle array of 1-9 or A-? to form first row
val widthIndex = 8
val width = widthIndex + 1
val all = (1 to 9).toList
val firstRow = List(Random.shuffle(1 to width).toList)
/*val test = List(List(1,2,3,4,5,6,7,8,9)) */
/*val c = List(List(0,2,3,4,5,6,7,8,9),List(1,2,3,4,5,6,7,8,9),List(2,2,3,4,5,6,7,8,9),
 List(3,2,3,4,5,6,7,8,9),List(4,2,3,4,5,6,7,8,9),List(5,2,3,4,5,6,7,8,9),
 List(6,2,3,4,5,6,7,8,9),List(7,2,3,4,5,6,7,8,9),List(8,2,3,4,5,6,7,8,9))*/
//Use to create list of existing column values
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
  println("Available " + available)
  println("Column: " + column)
  println("Group: " + group)
  println("Considering " + nextItem(0))
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
  println("Counter: " + counter)
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
      println("Row: " + runningRow)
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
//
val test = List(List(1,2,3,4,5,6,7,8,9))
createRow(List(), test, 0)
val t1 = System.nanoTime() 
println("Elapsed time: " + (t1 - t0) + " nanoseconds")



def createPuzzle(sudoku: List[List[Int]]): List[List[Int]] = {
  println(sudoku)
  if(sudoku.length == width) return sudoku 
  else {
    val nextRow = createRow(List(), sudoku, 0)
    val runningPuzzle = sudoku :+ nextRow
    createPuzzle(runningPuzzle)
  }
}




//createPuzzle(firstRow)
//print puzzle
def printPuzzle(start: List[List[Int]]): Unit = {
  val puzzle = createPuzzle(start)
  println("-----" * 9)
  for(i <- puzzle; n <- i){
    if(i.indexOf(n) == i.length - 1){
      print(" | ")
      print(n.toString)
      print(" | ")
      print("\n")
      println("-----" * 9)
    }
    else if(i.indexOf(n) == 3 || i.indexOf(n) == 6) {
      print(" || ")
      print(n.toString)
    }
    else{
    print(" | ")
    print(n.toString)
    print("")
    }
  }
}
//
//test
printPuzzle(firstRow)
//loop to form subsequent rows so that each does not conflict with all above rows
  //1--store available numbers in an array
  //2--store numbers in a placeholder value which filters out the numbers which already 
    //...exist in the same column, then select randomly from this array
  //3--loop to the next row and perform the same operation until the chart is complete
  //4--randomly move through the puzzle recursively and remove values
	//5--check whether more than one value could be placed in each section
    //...and backtrack if there could be more than one value 
