package calculator

object TweetLength {
  final val MaxTweetLength = 140

  def tweetRemainingCharsCount(tweetText: Signal[String]): Signal[Int] = {
    new Signal[Int]({
      println("---- current "+tweetLength(tweetText()))

      val remain = MaxTweetLength - tweetLength(tweetText())
      println("++++ remaining "+ remain)
      remain
    })
  }

  def colorForRemainingCharsCount(remainingCharsCount: Signal[Int]): Signal[String] = {
    new Signal[String]({
      val x: Int = remainingCharsCount()
      println(">>>>>> remaining is: "+x)
      var color = "orange"
      if (x >= 15) color = "green"
      else if (x < 0) color = "red"
      println("<<<<<<< returning color "+color)
      color
    })

//    x match {
//      case y if y > 15 => Signal("green")
//      case z if z < 0 => Signal("red")
//      case _ => Signal("orange")
//    }
  }

  /** Computes the length of a tweet, given its text string.
   *  This is not equivalent to text.length, as tweet lengths count the number
   *  of Unicode *code points* in the string.
   *  Note that this is still a simplified view of the reality. Full details
   *  can be found at
   *  https://dev.twitter.com/overview/api/counting-characters
   */
  private def tweetLength(text: String): Int = {
    /* This should be simply text.codePointCount(0, text.length), but it
     * is not implemented in Scala.js 0.6.2.
     */
    if (text.isEmpty) 0
    else {
      text.length - text.init.zip(text.tail).count(
          (Character.isSurrogatePair _).tupled)
    }
  }
}
