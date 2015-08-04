package sorting

object SelectionSort extends App {

  def minimum(head: Int, tail: List[Int]) : List[Int] = {
    tail match {
      case Nil => List(head)
      case nextHead :: nextTail => {
        if (head <= nextHead)
          head :: tail
        else
          nextHead :: minimum(head, nextTail)
      }
    }
  }

  def selectionSort(list: List[Int]) : List[Int] = {
    list match {
      case Nil => List()
      case head :: tail => minimum(head, selectionSort(tail))
    }
  }

  def selectionSortLoop(list: List[Int]) : List[Int] = {
    val intArray: Array[Int] = list.toArray
    for ( i <- 0 until intArray.length) {
      var min = i
      for (j <- (i+1) until intArray.length) {
        if(intArray(j) < intArray(min)) {
          min = j
        }
      }
      if (min != i) {
        val temp = intArray(i)
        intArray(i) = intArray(min)
        intArray(min) = temp
      }
    }
    intArray.toList
  }

  var list = List(5,2,4,6,1,3)
  var sortedList = selectionSort(list)
  println("Sorted List -> " + sortedList.toString() )

  sortedList = selectionSortLoop(list)
  println("Sorted List Loop -> " + sortedList.toString() )
}
