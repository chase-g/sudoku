import scala.util.Random
//randomly shuffle array of 1-9 or A-? to form first row

val widthIndex = 8
val width = widthIndex + 1
val firstRow = List(Random.shuffle(1 to width).toList)
//val testPuzzle = List(List(1,2,3,4,5,6,7,8,9), List(5,6,7,8,9,1,2,3,4), List(3, 5, 8, 6, 1, 9, 4, 2, 7), List(2, 7, 5, 3, 4, 8, 9, 6, 1), List(6, 8, 1, 7, 2, 4, 3, 9, 5)) 

//Use to create list of existing column values
def getColumn(index: Int, puzzle: List[List[Int]]) = { 
    val column = for(outer <- puzzle) yield {
      outer(index)
    }
    column
  }
//Use to create list of existing 3 x 3 group values
def getGroup(indexOut: Int, indexIn: Int, puzzle: List[List[Int]]) = {
  val groupX = {
    if(indexOut < 3) 3
    else if(indexOut < 6) 6
    else 9
  }
  val groupY = {
    if(indexIn < 3) 3
    else if(indexIn < 6) 6
    else 9
  }
    val group = for(outer <- puzzle; inner <- outer) yield {
     if(puzzle.indexOf(outer) < groupX && outer.indexOf(inner) < groupY) inner 
    }
  group
}
//Use to check potential new value against existing column values and then produce unique value
def nextValue(available: List[Int], index: Int, puzzle: List[List[Int]]): Int = {
  if(!available.isEmpty){
  val nextItem = Random.shuffle(available)
  println("Available nums: " + nextItem)
  val column = getColumn(index, puzzle)
  val group = getGroup(
  println("Column " + column)
    if (!column.contains(nextItem(0))) {
     // println("Using: " + nextItem(0))
      return nextItem(0)
    } else if(nextItem.length == 0) {
      return -1
    }
      else {
      nextValue(nextItem.drop(1), index, puzzle)
  }
 } else return -1
}
//test
//nextValue(available = List(2,3,1,4), index = 1, puzzle = List(List(1,2,3,4),List(2,4,1,3)))

//Create row of values which do not conflict with existing columns
def createRow(current: List[Int], nextNum: Int, puzzle: List[List[Int]]): List[Int] = {
  //println("Stack: " + stack)
 // println("Current list: " + current)
  val row: List[Int] = nextNum :: current
  //println("Applying num: " + row)
  //println(row)
  //if row is full, end and return row
  if(row.length == width) {
    val across = row
    println(across)
    return across 
    }
  //if not full, continue adding values to row
  else { 
    //Get next value from a list which has previously used numbers from the row filtered out
    val nextInt = nextValue((1 to width).toList.filter(x => !row.contains(x)), widthIndex - row.length, puzzle)
    //if nextInt is above 0 (i.e. did not have conflict) then add it to row recursively and continue cycle
    if(nextInt > 0) {
      println("Plan A")
        createRow(row, nextInt, puzzle)
    }  
    //if there is a conflict (-1 returned) then restart the row (later change this to backtracking one until repaired)
    else {
      println("Plan B")
      createRow(List(), nextValue((1 to width).toList, widthIndex, puzzle), puzzle)
    }
  }
}
//test
createRow(List(), nextValue((1 to width).toList, widthIndex, testPuzzle), testPuzzle)

def createPuzzle(puzzle: List[List[Int]]): List[List[Int]] = {
  if(puzzle.length == width) return puzzle
  else {
    val nextPuzzle = createRow(List(), nextValue((1 to width).toList, widthIndex, puzzle), puzzle) :: puzzle
    createPuzzle(nextPuzzle)
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