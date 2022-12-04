package playGround

object Entenhausen {
  def main(args: Array[String]): Unit = {

    val calories: List[(String, String, List[(String, Int)])] = {
    List(
      ("Donald Duck", "2022-01-01", List(("Frühstück", 800), ("Mittag", 700), ("Snack", 200), ("Abendbrot", 500))),
      ("Donald Duck", "2022-01-02",List(("Frühstück",700), ("Mittag", 650), ("Abendbrot", 520))),
      ("Donald Duck", "2022-01-03", List(("Frühstück", 800), ("Mittag", 700), ("Snack", 200), ("Abendbrot", 500), ("Snack", 150))),
      ("Donald Duck", "2022-01-04", List(("Frühstück", 850), ("Mittag", 900), ("Snack", 500), ("Snack", 400))),
      ("Donald Duck", "2022-01-05", List(("Frühstück", 600), ("Mittag", 700), ("Snack", 200), ("Abendbrot", 100))),
      ("Dagobert Duck", "2022-01-01", List(("Frühstück", 300), ("Mittag", 500), ("Snack", 100), ("Abendbrot", 200))),
      ("Dagobert Duck", "2022-01-02", List(("Frühstück", 200), ("Mittag", 300), ("Snack", 400), ("Abendbrot", 200))),
      ("Dagobert Duck", "2022-01-03", List(("Frühstück", 800), ("Mittag", 700), ("Snack", 200), ("Snack", 200))),
      ("Dagobert Duck", "2022-01-04", List(("Frühstück", 200), ("Mittag", 300), ("Snack", 200), ("Snack", 500))),
      ("Dagobert Duck", "2022-01-05", List(("Frühstück", 200), ("Mittag", 700), ("Abendbrot", 500))))
    }

    println("AllDaysWithMaxCalories: " + allDaysWithMaxCalories(calories))
    println("DayWithMaxCalories: " + dayWithMaxCalories(calories))
    println("DayWithMaxCaloriesGaertner: " + dayWithMaxCaloriesGaertner(calories))

    dayWithMaxCalories(calories)

    // (Name, Tag, Tageskalorienanzahl)
    def allDaysWithMaxCalories(l: List[(String, String, List[(String, Int)])]): List[(String, String, Int)] = {
      l
        .map({ case (name, day, list) => (name, day, reduceList(list)) })
        .groupBy(x => x._2)
        .map({case (k -> v) => (k -> reduceMaxCalories(v))})
        .map({case (k -> v) => Tuple3(v._1, k, v._3)}) to List
    }

    def dayWithMaxCalories(l: List[(String, String, List[(String, Int)])]): (String, String, Int) = {
      l
        .map({case (x) => Tuple3(x._1, x._2, countCalories(x._3))})
        .reduce((x, y) => {
          if (x._3 > y._3) x
          else y
        })
    }

    def dayWithMaxCaloriesGaertner(l: List[(String, String, List[(String, Int)])]): (String, String, Int) = {
      l.map(x => (x._1, x._2, x._3.map(_._2).sum)).
        reduce((a, b) => if (a._3 < b._3) b else a)
    }

    def reduceList(list: List[(String, Int)]): Int = {
      list.map((x => x._2)).sum
    }

    def reduceMaxCalories(l: List[(String, String, Int)]): (String, String, Int) = {
      l.reduce((x, y) => {
        if (x._3 > y._3) (x._1, x._2, x._3)
        else (y._1, y._2, y._3)
      })
    }

    def countCalories(l: List[(String, Int)]): Int = {
      l.reduce((x, y) => (x._1, x._2 + y._2))._2
    }
  }

}
