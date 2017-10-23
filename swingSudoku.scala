import scala.swing._
import scala.swing.event._
import java.awt.Dimension
import java.awt.Color._
import javax.swing.border._
import java.awt.Font._

class UI extends MainFrame {

import scala.util.Random
//randomly shuffle array of 1-9 or A-? to form first row
private val widthIndex = 8
private val width = widthIndex + 1
private val all = (1 to 9).toList
private val firstRow = List(Random.shuffle(1 to width).toList)

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
    if (!column.contains(nextItem(0)) && !group.contains(nextItem(0))) {
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
  if(currentRow.length != width){
    val avail = all.filter(x => !currentRow.contains(x)) //set available values
 //nextValue(available, across, down, puzzle)
    val nextNum = nextValue(avail, currentRow.length, puzzle.length, puzzle) //get nextValue with available values, across at current puzzle index length, down at index, on puzzle arg
 //changed from puzzle.length - 1
    if(nextNum > 0){ //If nextNum is positive 
      val backtrack = counter + 1
      val runningRow = currentRow :+ nextNum//append it to curentRow
      createRow(runningRow, puzzle, backtrack) //call createRow recursively with value added to row
    } else { //otherwise backtrack to prior value when calling createRow recursively
      if(counter > 0){
      val runningRow = currentRow.slice(0, currentRow.length - (counter / 2))
      createRow(runningRow, puzzle, 0)
      } else {
        createRow(List(), puzzle, 0)
      }
    }
  } else return currentRow
}

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

private val solution = createPuzzle(firstRow)
private val sudoku = createGaps(solution)
printPuzzle(sudoku) 

  def restrictHeight(s: Component) {
    s.maximumSize = new Dimension(Short.MaxValue, s.preferredSize.height)
  }
  title = "Sudoku"
  
  private val btnSize = new Dimension(50, 50)
  private val emerald: Color = new Color(0, 153, 51)
  private val burntOrange: Color = new Color(230, 115, 0)
  private val oceanBlue: Color = new Color(0, 115, 230)
  private val myFont: Font = new Font("serif", BOLD, 18);
  private var picked = ""
  
contents = new BoxPanel(Orientation.Vertical) {
   contents += new Label("Chase's Sudoku Generator"){
     font = new Font("serif", BOLD, 24);
   }
   contents += Swing.VStrut(30)
      for(i <- 0 until sudoku.length) {
            contents += new BoxPanel(Orientation.Horizontal){
            for(n <- 0 until sudoku(i).length) {
              contents += new BoxPanel(Orientation.Horizontal){
              contents += new Button(sudoku(i)(n)) {
                 reactions += {
                   case ButtonClicked(_) => {
                     if(picked == solution(i)(n)) 
                     text = picked
                     foreground = oceanBlue
                   }
                 }
                 minimumSize = btnSize
                 maximumSize = btnSize
                 preferredSize = btnSize
                 font = myFont
                 if((n > 2 && n < 6 && i < 3) || (n > 2 && n < 6 && i > 5)){
                 foreground = emerald
                 } else if((i > 2 && i < 6 &&  n < 3) || (i > 2 && i < 6 && n > 5)) {
                   foreground = burntOrange
                 }
                 else {
                   foreground = java.awt.Color.black
                 }
              }
              if(n == 2 || n == 5) contents += Swing.HStrut(15)
            }
              
          } 
      }
            
          if(i == 2 || i == 5) contents += Swing.VStrut(15)
    }
   //selected
      /*
      contents += new BoxPanel(Orientation.Horizontal){
      contents += Swing.VStrut(30)
      contents += new Label("Selected: " + picked) {
        listenTo(Button)
        reactions += {
          case ButtonClicked(_) => {
            text = "Selected: " + picked
          }
        }
      }
      }*/
        
      //options class
      class Option (val num: String) extends Button {
        private val btnSize = new Dimension(50, 50)
        private val myFont: Font = new Font("serif", BOLD, 18);
        text = num
        minimumSize = btnSize
        maximumSize = btnSize
        preferredSize = btnSize
        font = myFont
        reactions += {
            case ButtonClicked(_) => {
              picked = num.toString
            }
          }
      }
      
      contents += Swing.VStrut(30)
      contents += new Label("Options:")
      contents += new BoxPanel(Orientation.Horizontal){
        for(t <- 1 to 9){
        contents += new BoxPanel(Orientation.Horizontal){
        contents += Swing.HStrut(3)
        contents += new Option(t.toString)
      }
    }
      }
   
    for (e <- contents){
      e.xLayoutAlignment = 0.0
      border = Swing.EmptyBorder(30, 30, 30, 30)
    }
      
  }
}


object swingSudoku {
  def main(args: Array[String]) {
    val ui = new UI
    ui.visible = true
  }
}
