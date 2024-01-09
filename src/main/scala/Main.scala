import com.github.plokhotnyuk.jsoniter_scala.core.*
import com.github.plokhotnyuk.jsoniter_scala.macros.*
import domain.{Parameter, Vote, VoteResult}

import scala.io.Source

given parameterCodec: JsonValueCodec[Parameter] = JsonCodecMaker.make
given voteCodec: JsonValueCodec[Vote] = JsonCodecMaker.make
given voteArrayCodec: JsonValueCodec[Array[Vote]] = JsonCodecMaker.make

def readFromFile(filename: String): String = {
  val file = Source.fromFile(filename)
  val json = file.getLines().mkString
  file.close()
  json
}

def prettyVoteResult(voteResult: VoteResult): String = {
  s"Nb of votes : ${voteResult.counterVote}\n" +
    s"Nb of votes 1 : ${voteResult.nbVote1}\n" +
    s"Nb of votes 2 : ${voteResult.nbVote2}\n" +
    s"Nb of votes 3 : ${voteResult.nbVote3}\n" +
    s"Nb of votes 4 : ${voteResult.nbVote4}\n"
}

@main def main(): Unit = {

  val parameterFilename = "param.json"
  val voteFilename = "vote.json"
  val param = readFromString[Parameter](readFromFile(parameterFilename))
  val votes: Array[Vote] = readFromString[Array[Vote]](readFromFile(voteFilename))
  val voteResults = votes.foldLeft(Map(param.activity1 -> VoteResult(), param.activity2 -> VoteResult(), param.activity3 -> VoteResult(), param.mystery -> VoteResult())) {
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
  val maxResult = voteResults.reduce { (left, right) =>
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
      case _ => left
    }
  }

  println("############################################################################")
  println("----------------------------------------------------------------------------")
  println("|                              THE NOMINATED WERE                          |")
  println("----------------------------------------------------------------------------")
  println("|                                                                          |")
  println(s"1. ${param.activity1}")
  println(s"2. ${param.activity2}")
  println(s"3. ${param.activity3}")
  println(s"4. ${param.mystery}")
  println("|                                                                          |")
  println("----------------------------------------------------------------------------")
  println("|                                 RESULTS                                  |")
  println("----------------------------------------------------------------------------")
  voteResults.foreach((activity, voteResult) => println(s"Activity : $activity \n${prettyVoteResult(voteResult)}"))
  println("----------------------------------------------------------------------------")
  println("|                            AND THE WINNER IS                             |")
  println("----------------------------------------------------------------------------")
  println("|                                                                          |")
  println(s"                             ${maxResult._1}                              ")
  println("|                                                                          |")
  println("|                                                                          |")
  println("----------------------------------------------------------------------------")
  println("############################################################################")

}
