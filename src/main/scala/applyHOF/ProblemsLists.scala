package applyHOF

import list.implementation._
import list.traits.IntList

object ProblemsLists {

  /**
    *
    * Given a number i that should be duplicated a number of times
    * returns an IntList that contains the duplicated i
    *
    * E.x. duplicateNum(4,3)
    * -> SinglyLinkedList(4, 4, 4, 4)
    *
    * @param i     number to duplicate
    * @param times number of duplicates
    * @return List of duplicated numbers
    */
  def duplicateNum(i: Int, times: Int): IntList = times match {
    case 0 => Empty
    case _ => Cons(i, duplicateNum(i, times - 1))
  }

  // fails on duplicateNum1(5, 0)
  def duplicateNum1(i: Int, times: Int): IntList = {
        times match {
          case 1 => Cons(i, Empty)
          case _ => Cons(i, duplicateNum(i, times - 1))
        }
  }
  // fails on duplicateNum1(5, 0)
  def duplicateNum2(i: Int, times: Int): IntList =  {
    times match {
      case _ if (times > 1) => Cons(i, duplicateNum(i, times - 1))
      case _ => Cons(i, Empty)
    }
  }

  /**
    *
    * Given an IntList l that contains even and odd numbers
    * All even numbers of the list should be duplicated an number of times
    * returns an IntList that contains the all duplicated even numbers and the
    * remaining odd numbers in the same order as they occur in the origin list
    *
    * E.x. duplicateEqualNumbers(3,SinglyLinkedList(1,4,3,5,8))
    * -> SinglyLinkedList(1, 4, 4, 4, 3, 5, 8, 8, 8)
    *
    * @param times number of duplicates
    * @param l     IntList that should be processed
    * @return IntList that contains the duplicates and all other nums
    */
  def duplicateEqualNumbers(times: Int, l: IntList): IntList = l match {
    case Empty => Empty
    case _ if (l.head % 2 == 0) => duplicateEqualNumbers(times, l.tail).prefix(duplicateNum(l.head, times));
    case _ => Cons(l.head, duplicateEqualNumbers(times, l.tail))
  }

  def duplicateEqualNumbersComplicated(times: Int, l: IntList): IntList = {
        def iterate(dupList: IntList, index: Int): IntList = {
          def duplicate(list: IntList, times: Int): IntList = {
            if (times == 0) {
              list
            } else {
              duplicate(list.append(l.get(index)), times - 1)
            }
          }

          if (index < l.size) {
            l.get(index) match {
              case odd if (odd % 2 != 0) => iterate(dupList.append(odd), index + 1)
              case even if (even % 2 == 0) => iterate(duplicate(dupList, times), index + 1)
            }
          } else {
            dupList
          }
        }

        iterate(SinglyLinkedIntList(), 0)
  }

  /*

  Harder Problems - Optional

   */

  /**
    *
    * Given a amount of money and a list of coin values,
    * returns the number of possible ways that the change can be returned using
    * coins of those values.
    *
    * E.x. countChange(4,SinglyLinkedList(1,2)
    * has 3 solutions -> {1+1+1+1 ; 2+2 ; 1+2+1}
    *
    * @param money total amount of change to return
    * @param coins possible coins
    * @return number of possible ways the change can be returned
    */
  def countChange(Money: Int, coins: IntList): Int = ???


}
