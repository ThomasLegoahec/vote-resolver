import com.github.plokhotnyuk.jsoniter_scala.core.*
import com.github.plokhotnyuk.jsoniter_scala.macros.*
import domain.{Parameter, Vote, VoteResult}

import scala.io.Source

given parameterCodec: JsonValueCodec[Parameter] = JsonCodecMaker.make

def readFromFile(filename: String): Seq[String] = {
  val file = Source.fromFile(filename)
  val json = file.getLines().toSeq
  file.close()
  json
}

def parseVotes(rawVotes: Seq[String]): Seq[Vote] = {

  val pattern = """\s*([1-4])\/([1-4])\/([1-4])\/([1-4])\s*(\w+)""".r

  rawVotes.map(vote => {
    val pattern(vote1, vote2, vote3, vote4, name) = vote
    Vote(vote1, vote2, vote3, vote4, name)
  })
}

def prettyVoteResult(voteResult: VoteResult, activity: String, bonusActivity: String): String = {
  s"Activity : $activity \n" +
    s"Nb of votes : ${voteResult.counterVote} ${
      if (activity == bonusActivity) {
        "(+1 from bonus)"
      } else ""
    } \n" +
    s"Nb of votes 1 : ${voteResult.nbVote1}\n" +
    s"Nb of votes 2 : ${voteResult.nbVote2}\n" +
    s"Nb of votes 3 : ${voteResult.nbVote3}\n" +
    s"Nb of votes 4 : ${voteResult.nbVote4}\n"
}

def computeVotes(votes: Seq[Vote], paramMap: Map[String, String], activityWithBonusNumber: String): Map[String, VoteResult] = {
  val voteResults = votes.foldLeft(paramMap.keys.map(activityNumber => activityNumber -> VoteResult()).toMap) {
    (counters, vote) =>
      val voteResult1 = counters(vote.vote1)
      val voteResult2 = counters(vote.vote2)
      val voteResult3 = counters(vote.vote3)
      val voteResult4 = counters(vote.vote4)
      Map(vote.vote1 -> voteResult1.copy(counterVote = voteResult1.counterVote + 4, nbVote1 = voteResult1.nbVote1 + 1),
        vote.vote2 -> voteResult2.copy(counterVote = voteResult2.counterVote + 3, nbVote2 = voteResult2.nbVote2 + 1),
        vote.vote3 -> voteResult3.copy(counterVote = voteResult3.counterVote + 2, nbVote3 = voteResult3.nbVote3 + 1),
        vote.vote4 -> voteResult4.copy(counterVote = voteResult4.counterVote + 1, nbVote4 = voteResult4.nbVote4 + 1))
  }
  val activityWithBonus = voteResults(activityWithBonusNumber)
  val activityWithBonusWeighted = activityWithBonus.copy(counterVote = activityWithBonus.counterVote + 1)
  voteResults + (activityWithBonusNumber -> activityWithBonusWeighted)
}

def computeWinner(activityWithBonusNumber: String, voteResultsWeighted: Map[String, VoteResult]): (String, VoteResult) = {
  voteResultsWeighted.reduce { (left, right) =>
    val leftVote = left._2
    val rightVote = right._2
    (leftVote, rightVote) match {
      case (leftVote, rightVote) if leftVote.counterVote > rightVote.counterVote => left
      case (leftVote, rightVote) if leftVote.counterVote < rightVote.counterVote => right
      case (leftVote, rightVote) if leftVote.counterVote == rightVote.counterVote && leftVote.nbVote1 > rightVote.nbVote1 => left
      case (leftVote, rightVote) if leftVote.counterVote == rightVote.counterVote && leftVote.nbVote1 < rightVote.nbVote1 => right
      case (leftVote, rightVote) if leftVote.counterVote == rightVote.counterVote && leftVote.nbVote2 > rightVote.nbVote2 => left
      case (leftVote, rightVote) if leftVote.counterVote == rightVote.counterVote && leftVote.nbVote2 < rightVote.nbVote2 => right
      case (leftVote, rightVote) if leftVote.counterVote == rightVote.counterVote && leftVote.nbVote3 > rightVote.nbVote3 => left
      case (leftVote, rightVote) if leftVote.counterVote == rightVote.counterVote && leftVote.nbVote3 < rightVote.nbVote3 => right
      case (leftVote, rightVote) if leftVote.counterVote == rightVote.counterVote && leftVote.nbVote4 > rightVote.nbVote4 => left
      case (leftVote, rightVote) if leftVote.counterVote == rightVote.counterVote && leftVote.nbVote4 < rightVote.nbVote4 => right
      case (leftVote, rightVote) if left._1 == activityWithBonusNumber => left
      case (leftVote, rightVote) if right._1 == activityWithBonusNumber => right
      case _ => left
    }
  }
}
@main def main(): Unit = {

  val parameterFilename = "param.json"
  val param = readFromString[Parameter](readFromFile(parameterFilename).mkString)
  val paramMap = Map("1" -> param.activity1, "2" -> param.activity2, "3" -> param.activity3, "4" -> param.activity4)
  val activityWithBonusNumber = paramMap.filter((activityNumber: String, activityName: String) => activityName == param.activityWithBonus).keys.head

  val voteFilename = "votes.txt"
  val rawVotes = readFromFile(voteFilename)
  val votes: Seq[Vote] = parseVotes(rawVotes)
  
  val voteResultsWeighted = computeVotes(votes, paramMap, activityWithBonusNumber)
  
  val winner = computeWinner(activityWithBonusNumber, voteResultsWeighted)

  println("############################################################################")
  println("----------------------------------------------------------------------------")
  println("|                              THE NOMINATED WERE                          |")
  println("----------------------------------------------------------------------------")
  println("|                                                                          |")
  println(s"1. ${param.activity1}")
  println(s"2. ${param.activity2}")
  println(s"3. ${param.activity3}")
  println(s"4. ${param.activity4}")
  println("|                                                                          |")
  println(s"Bonus active on : ${param.activityWithBonus}")
  println("|                                                                          |")
  println("----------------------------------------------------------------------------")
  println("|                                 RESULTS                                  |")
  println("----------------------------------------------------------------------------")
  voteResultsWeighted.foreach((activity, voteResult) => println(prettyVoteResult(voteResult, paramMap(activity), param.activityWithBonus)))
  println("----------------------------------------------------------------------------")
  println("|                            AND THE WINNER IS                             |")
  println("----------------------------------------------------------------------------")
  println("|                                                                          |")
  println(s"                             ${paramMap(winner._1)}                        ")
  println("|                                                                          |")
  println("|                                                                          |")
  println("----------------------------------------------------------------------------")
  println("############################################################################")

}
