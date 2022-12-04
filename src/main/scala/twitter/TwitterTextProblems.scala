package twitter

import twitter.models.Tweet

import scala.collection.immutable.HashSet

object TwitterTextProblems {

  /*
    * Extracts all words from a line
    *
    * 1. Removes all characters which are not letters (A-Z or a-z)
    * 2. Shifts all words to lower case
    * 3. Extracts all words and put them into a list of strings
    */
  def getWords(line: String): List[String] = {

    val lower = line.toLowerCase.replaceAll("[^a-z]", " ")
    lower.split(" ").filter(_ != "").toList
  }

  /*
     * Extracts all words from a List containing line number and line tuples
     * The words should be in the same order as they occur in the source document
     *
     * Hint: Use the flatMap function
   */
  def getAllWords(l: List[(Any, String)]): List[String] = {
    l.flatMap{case (a,y) => getWords(y)}
  }

  /*
     *  Gets a list of words and counts the occurrences of the individual words
  */
  def countWords(l: List[String]): List[(String, Int)] = {
    l.groupBy(identity).map { case (k,v) => (k, v.size)} to List
  }

  /*
    * Gets a list of the most 10 Used Words in a list of Tweets
    * It should be sorted by:
    * - number of occurences (first criteria descending)
    * - alphabetically (second criteria ascending)
   */
  def getMost10UsedWords(l:List[Tweet]):List[(String,Int)]= {
    // https://stackoverflow.com/questions/59247874/scala-how-to-sort-tuples-by-both-attributes-in-different-order
    countWords(l.map(Tweet => Tweet.text).flatMap(s => getWords(s))).sorted((x: (String, Int), y: (String, Int)) => {
      if (y._2 > x._2) 1
      else if (y._2 < x._2) -1
      else x._1.compareTo(y._1)
    }).take(10)
  }

  /*
    * The data set should be processed by
    *  - cleaning all words that are in the stop word list
    *  - cleaning all words that have less than 3 letters
    *  - every word in a tweet should only be counted once (delete the word duplicates in a tweet)
    * Function should return a list of tuples (tweet ID, Set of Words that are within the text
    *
  */
  def prepareData(l:List[Tweet], stopW:HashSet[String]):List[(Long,Set[String])]= {

//    l.map(tweet => (tweet.tweet_id, getWords(tweet.text))).map({case(id, wordList) => (id, wordList.filter(word => !stopW.contains(word) && word.size >= 3) to Set)})
//    l.map(Tweet => Tuple2(Tweet.tweet_id, Set(Tweet.text))).map({case (l, s) => Tuple2(l, s.flatMap(s => getWords(s)).filter(s => s.length >= 3).filter(s => !stopW.contains(s)))})

    // with diff:
    l.map(Tweet => Tuple2(Tweet.tweet_id, Set(Tweet.text))).map({case (l, s) => Tuple2(l, s.flatMap(s => getWords(s)).filter(s => s.length >= 3).diff(stopW))})
  }

  /*
   * Gets a list of the most 10 Used Words in a list of Tweets
   * Before counting the words the list should be cleaned by invoking prepareData
   * It should be sorted by:
   * - number of occurences (first criteria descending)
   * - alphabetically (second criteria ascending)
  */
  def getMost10UsedWordsCleaned(l:List[Tweet], stopW:HashSet[String]):List[(String,Int)]= {
    prepareData(l, stopW).flatMap(_._2).groupBy(identity).map({ case (k -> v) => (k, v.size) }).toList.sorted((x: (String, Int), y: (String, Int)) => {
      if (y._2 > x._2) 1
      else if (y._2 < x._2) -1
      else x._1.compareTo(y._1)
    }).take(10)
  }

  /*
  * Gets a list of the 10 least used words in a list of Tweets
  * Before counting the words the list should be cleaned by invoking prepareData
  * It should be sorted by:
  * - number of occurences (first criteria descending)
  * - alphabetically (second criteria ascending)
  *
  * Filter also all words that are used less the 5 times
   */
  def getLeast10UsedWordsCleaned(l:List[Tweet], stopW:HashSet[String]):List[(String,Int)]= {

    prepareData(l, stopW).flatMap(_._2).groupBy(identity).map({ case (k -> v) => (k, v.size) }).toList.filter(_._2 >= 5)
      .sorted((x: (String, Int), y: (String, Int)) => {
        if (y._2 < x._2) 1
        else if (y._2 > x._2) -1
        else x._1.compareTo(y._1)
      }).take(10)

//    println(prepareData(l, stopW).flatMap(_._2).groupBy(identity).map({ case (k -> v) => (k, v.size) }).toList.filter(_._2 >= 5)
//      .sorted((x: (String, Int), y: (String, Int)) => {
//      if (y._2 < x._2) 1
//      else if (y._2 > x._2) -1
//      else x._1.compareTo(y._1)
//    }).take(10))
//
//    l.take(100).map(t => (t.partei, t.tweet_id.toInt)) to List
  }

  /*
    * Gets all words of a list of Tweets combined with the tweet_ids where they are occuring
    * The function should return a set of tuples where the first element is the id and the second a that is in the tweet
     */
  def getAllWordsWithIndex(l: List[Tweet]): Set[(Long, String)] = {
//    println(l.map(Tweet => Tuple2(Tweet.tweet_id, Set(Tweet.text))).map({case (l, s) => Tuple2(l, s.flatMap(s => getWords(s)))}))
//    println(l.map(Tweet => HashSet(Tweet.tweet_id, getWords(Tweet.text))).map({case x => concatenate(x)}))
//    println(l.map(Tweet => Tuple2(Tweet.tweet_id , getWords(Tweet.text))).flatMap({case (x,y) => y.map(s => Tuple2(x, s))}) to Set)

    l.map(Tweet => Tuple2(Tweet.tweet_id , getWords(Tweet.text))).flatMap({case (tweet_id,wordList) => wordList.map(word => Tuple2(tweet_id, word))}) to Set
  }

  /*
  * Function should create an Inverse Index
  * It should return a Map the words as the key element and a set of tweet ids where the word is contained.
   */
  def createInverseIndex(l: Set[(Long, String)]): Map[String, Set[Long]] = {
//    l.groupBy(_._2).map({case (word, set) => (word, set.map({case(l, s) => (l)}))})

    l.groupBy({case(x,y) => y}).map({case (k, v) => (k, v.map({case (k, v) => k}))})
  }

  /*
   * The Functions gets a list of words and returns a set of tweet ids where at least one
   * of the word occurs
   * Use the inverse index for calculating the or-Operation.
  */
  def orConjunction(words: List[String], invInd: Map[String, Set[Long]]): Set[Long] = {
    words.flatMap(word => invInd.get(word)).flatten to Set
  }

  /*
  * The Functions gets a list of words and returns a set of tweet ids where all of the words occur
 * Use the inverse index for calculating the and-Operation.
   */
  def andConjunction(words: List[String], invInd: Map[String, Set[Long]]): Set[Long] = {
    words.map(word => (word, invInd.get(word))).map(x => x._2.getOrElse(Set())).reduce((a, b) => a intersect b)
  }

  def andConjunctionComplicated(words: List[String], invInd: Map[String, Set[Long]]): Set[Long] = {
    // return empty Set if one of the words doesn't appear in any tweet
    if(words.map(word => invInd.get(word)).contains(None)) return Set()
    else {
      // 1. eine Liste mit den IDs aller Tweets erstellen, in denen die gesuchten Wörter vorkommen.
      // 2. diese IDs mit groupBy zusammen fassen und den count der IDs ermitteln: Map [tweet_id -> so viele der gesuchten Wörter kommen in diesem Tweet vor]
      // 3. nach den IDs filtern, in denen alle gesuchten Wörter vor kommen -> diese werden retourniert                                                                                                                                       TODO == oder >=?
      words.flatMap(word => invInd.get(word)).flatten.groupBy(identity).map({case(long, list) => (long, list.size)}).filter({case (long, int) => int >= words.size}).map({case (long, int) => long}) to Set
    }
  }

  def andConjunctionOli(words: List[String], invInd: Map[String, Set[Long]]): Set[Long] = {
    if (words.forall(x => invInd.contains(x))) {
      words.flatMap(word => invInd.get(word)).reduceLeft(_.intersect(_))
    } else {
      Set()
    }
  }

  /**************************************************************************
    *    Helper Functions
    *************************************************************************/

  def getTweet(id:Long, tweets:List[Tweet]):Option[Tweet]={

    /*
    * Gets a tweet from a list of tweets by id
     */
    tweets match{
      case Nil => None
      case x::xs if (x.tweet_id==id) => Some(x)
      case _ => getTweet(id, tweets.tail)
    }
  }

}
