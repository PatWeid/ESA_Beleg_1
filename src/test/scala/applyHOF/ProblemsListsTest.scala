package applyHOF

import list.implementation._
import org.scalatest.funsuite.AnyFunSuite


// Gruppenpartner: Oliver Thomaschewski
class ProblemsListsTest extends AnyFunSuite{

  test("duplicate Num") {
    assert(ProblemsLists.duplicateNum(5, 3) === SinglyLinkedIntList(5, 5, 5))
  }

  test("duplicate Equal Numbers") {
    assert(ProblemsLists.duplicateEqualNumbers(3, SinglyLinkedIntList(5, 4, 7, 2)) ===
      SinglyLinkedIntList(5, 4, 4, 4, 7, 2, 2, 2))
  }

  test("duplicate Equal Numbers Complicated") {
    assert(ProblemsLists.duplicateEqualNumbersComplicated(3, SinglyLinkedIntList(5, 4, 7, 2)) ===
      SinglyLinkedIntList(5, 4, 4, 4, 7, 2, 2, 2))
  }

  // not implemented yet due to optional task...
//  test("testCountChange") {
//    assert(ProblemsLists.countChange(4, SinglyLinkedIntList(1, 2)) === 3)
//  }
}
