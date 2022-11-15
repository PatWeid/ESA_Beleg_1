package twitter

import twitter.models.Tweet

import scala.collection.immutable
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
  def getAllWords(l: List[(Any, String)]): List[String] = ???

  /*
     *  Gets a list of words and counts the occurrences of the individual words
  */
  def countWords(l: List[String]): List[(String, Int)] = ???

  /*
    * Gets a list of the most 10 Used Words in a list of Tweets
    * It should be sorted by:
    * - number of occurences (first criteria descending)
    * - alphabetically (second criteria ascending)
   */
  def getMost10UsedWords(l:List[Tweet]):List[(String,Int)]= ???

  /*
    * The data set should be processed by
    *  - cleaning all words that are in the stop word list
    *  - cleaning all words that have less than 3 letters
    *  - every word in a tweet should only be counted once (delete the word duplicates in a tweet)
    * Function should return a list of tuples (tweet ID, Set of Words that are within the text
    *
  */
  def prepareData(l:List[Tweet], stopW:HashSet[String]):List[(Long,Set[String])]= ???

  /*
   * Gets a list of the most 10 Used Words in a list of Tweets
   * Before counting the words the list should be cleaned by invoking prepareData
   * It should be sorted by:
   * - number of occurences (first criteria descending)
   * - alphabetically (second criteria ascending)
  */
  def getMost10UsedWordsCleaned(l:List[Tweet], stopW:HashSet[String]):List[(String,Int)]= ???

  /*
  * Gets a list of the 10 least used words in a list of Tweets
  * Before counting the words the list should be cleaned by invoking prepareData
  * It should be sorted by:
  * - number of occurences (first criteria descending)
  * - alphabetically (second criteria ascending)
  *
  * Filter also all words that are used less the 5 times
   */
  def getLeast10UsedWordsCleaned(l:List[Tweet], stopW:HashSet[String]):List[(String,Int)]= ???

  /*
  * Gets all words of a list of Tweets combined with the tweet_ids where they are occuring
  * The function should return a set of tuples where the first element is the id and the second a that is in the tweet
   */
  def getAllWordsWithIndex(l: List[Tweet]): Set[(Long, String)] = ???

  /*
  * Function should create an Inverse Index
  * It should return a Map the words as the key element and a set of tweet ids where the word is contained.
   */
  def createInverseIndex(l: Set[(Long, String)]): Map[String, Set[Long]] = ???

  /*
   * The Functions gets a list of words and returns a set of tweet ids where at least one
   * of the word occurs
   * Use the inverse index for calculating the or-Operation.
  */
  def orConjunction(words: List[String], invInd: Map[String, Set[Long]]): Set[Long] = ???

  /*
  * The Functions gets a list of words and returns a set of tweet ids where all of the words occur
 * Use the inverse index for calculating the and-Operation.
   */
  def andConjunction(words: List[String], invInd: Map[String, Set[Long]]): Set[Long] = ???

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