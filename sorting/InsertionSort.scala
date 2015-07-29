package sorting
// To compile while in the root folder scalac sorting/InsertionSort.scala
// To execute scala sorting.InsertionSort
// It prints out something like: Sorted List -> List(1, 2, 3, 4, 5, 6)
object InsertionSort extends App {

    def insert(head: Int, tail: List[Int]) : List[Int] = {
      tail match {
        case Nil => List(head)
        case nextHead :: nextTail => if (head <= nextHead) head :: tail else nextHead :: insert(head, nextTail)
      }
    }

    def insertionSort(list: List[Int]) : List[Int] = {
      list match {
          case Nil => List()
          case head :: tail => insert(head, insertionSort(tail))
      }
    }

    val list = List(5,2,4,6,1,3)
    val sortedList = insertionSort(list)
    println("Sorted List -> " + sortedList.toString() )
}
