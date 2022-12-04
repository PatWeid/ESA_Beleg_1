package list.implementation

import list.traits.IntList

/**
  * A companion object for the singly linked list.
  * This enables creating lists list this: val list = SinglyLinkedIntList(1,2,3)
  * which results in Cons(1,Cons(2,Cons(3,Empty))))
  */
object SinglyLinkedIntList {


  /** The apply function is a special function in scala.
    * It can be invoked with SinglyLinkedIntList.apply(args) or simply SinglyLinkedIntList(args).
    * This particular implementation of it is also a variadic function, i.e.
    * a function which accepts one or more arguments of the same type (integers) as parameters.
    */
  //inside this method xs is of type Seq[int]
  def apply(xs: Int*): SinglyLinkedIntList = xs match {
    case Seq() => Empty
    //: _* results in the sequence being passed as multiple parameters - (1,2,3) instead of Seq[Int]{1,2,3}
    case _ => Cons(xs.head, SinglyLinkedIntList(xs.tail: _*))
  }
}

abstract class SinglyLinkedIntList extends IntList {

  override def prefix(other: IntList): IntList = other match{
    case Empty => this
    case Cons(h,t) => Cons(h,prefix(t)) 
  }
  override def size: Int = this match{
    case Empty => 0
    case _ => 1+tail.size
  }

  override def map(mapFunc: Int => Int): IntList = this match{
    case Empty => Empty
    case Cons(_,_) => Cons(mapFunc(head), tail.map(mapFunc))
        
  }

  override def filter(filterFunc: Int => Boolean): IntList =  this match{
    case Empty => Empty
    case Cons(_,_) => if (filterFunc(head)) Cons(head, tail.filter(filterFunc)) else tail.filter(filterFunc)
  }

  override def forAll(predicateFunc: Int => Boolean): Boolean = this match {
    case Cons(_,Empty) => if (predicateFunc(head)) true else false
    case Cons(_,_) => predicateFunc(head) && tail.forAll(predicateFunc)
  }

  // https://stackoverflow.com/questions/40827710/scala-fold-right-and-fold-left
  override def foldLeft(initial: Int)(reduceFunc: (Int, Int) => Int): Int = this match {
    case Cons(_,_) => tail.foldLeft(reduceFunc(initial, head))(reduceFunc)
    case Empty => initial
  }

  override def reduceLeft(reduceFunc: (Int, Int) => Int): Int = this match {
    case Cons(head, Cons(_, Empty)) => reduceFunc(head, tail.head)
    case Cons(head, tail) => Cons(reduceFunc(head, tail.head), tail.tail).reduceLeft(reduceFunc)
  }

  override def foldRight(initial: Int)(reduceFunc: (Int, Int) => Int): Int = this match {
    case Empty => initial
    case Cons(_,_) => reduceFunc(head, tail.foldRight(initial)(reduceFunc))
  }

  override def reduceRight(reduceFunc: (Int, Int) => Int): Int = this match {
    case Cons(_, Empty) => head
    case Cons(head, tail) => reduceFunc(head, tail.reduceRight(reduceFunc))
  }

  override def insertSorted(elem: Int): IntList = this match {
    case Cons(_, Empty) => Cons(head, Cons(elem, Empty))
    case _ if (head > elem) => Cons(elem, Cons(head, tail))
    case _ if (tail.head > elem) => Cons(head, Cons(elem, tail))
    case _ if (head < elem) => Cons(head, tail.insertSorted(elem))
  }

  override def insertionSort: IntList = {
    def insert(x: Int, list: IntList): IntList = list match {
      case Empty => Cons(x, Empty)
      case Cons(h, t) => if (x <= h) Cons(x, list) else Cons(h, insert(x, t))
    }

    def sort(list: IntList): IntList = list match {
      case Empty => Empty
      case Cons(h, t) => insert(h, sort(t))
    }

    sort(this)
  }
}