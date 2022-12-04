package playGround



object PlayGround {
  def main(args: Array[String]): Unit = {
    println("HelloWorld")

    // finding average using map and reduce
    val l = List(1, 5, 7, 8)
    val reduced = l.map(x => (x, 1)).reduce((a, b) => (a._1 + b._1, a._2 + b._2)) // (21,4)
    val avg = reduced._1 / reduced._2.toFloat // 21/4 = 5,25
    println("average with reduce and map: " + avg)

    // reduce and fold with integer
    val l2 = List(1, 2, 3,4,5)
    println("reduce and fold with " + l2)
    println("reduce _+_: " + l2.reduce(_+_))
    println("reduce sum: " + l2.sum)
    println("reduce right x+y: " + l2.reduceRight((x,y) => x+y))
    println("reduce right _+_: " + l2.reduceRight(_+_))
    println("reduce right x-y: " + l2.reduceRight((x,y) => x-y))
    println("reduce left x-y: " + l2.reduceLeft((x,y) => x-y))
    println("foldLeft (5)(x+y) " + l2.foldLeft(5)((x,y) => x+y))

    // foldLeft Range
    println("foldLeft 0-100" + ( 1 to 100 ).foldLeft(0)( _-_ ))
    val l3 = List((0,1),(0,2),(0,3))
    val l4 = l3.foldLeft(List[Int]())((accumulator, tuple) => accumulator.appended(tuple._2))
    println("foldLeft " + l3 + " second Element: " + l4)

    // https://www.baeldung.com/scala/folding-lists
    case class Person(name: String, sex: String)
    val persons = List(Person("Thomas", "male"), Person("Sowell", "male"), Person("Liz", "female"))
    val foldedList = persons.foldLeft(List[(String, String)]()){ (acc, person) =>
      val title = person.sex match {
        case "male" => "Mr."
        case "female" => "Mrs."
      }
      acc.appended((title,person.name))
    }
    println("foldedLeft " + persons + " filtered: "+ foldedList)
  }


}
