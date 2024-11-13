import domain.{Parameter, Vote}
import org.scalatest.GivenWhenThen
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.must.Matchers
import org.scalatest.matchers.should.Matchers.should

import scala.io.Source

class VoteResolverTest extends AnyFlatSpec with GivenWhenThen with Matchers {

  "parseVotes" should "parse file and return Votes" in {

    Given("a file with votes")
    val source = Source.fromURL(getClass.getResource("/votes.txt"))
    val json = source.getLines().toSeq
    source.close()

    When("parsing the content of the file")
    val votes: Seq[Vote] = parseVotes(json)

    Then("votes should be available")
    votes.size should be(13)

    val firstVote = votes.head
    firstVote.vote1 should be("4")
    firstVote.vote2 should be("1")
    firstVote.vote3 should be("2")
    firstVote.vote4 should be("3")
    firstVote.name should be("Jim")

  }

  "computeVotes" should "parse file and compute results" in {

    Given("a file with votes parsed")
    val source = Source.fromURL(getClass.getResource("/votes.txt"))
    val json = source.getLines().toSeq
    source.close()
    val votes: Seq[Vote] = parseVotes(json)
    And("parameters with activity name and bonus")
    val param = Parameter("marais salants de guérande","brassage de binouze","eventyr halloween","fly a 75 boules","brassage de binouze")
    val paramMap = Map("1" -> param.activity1, "2" -> param.activity2, "3" -> param.activity3, "4" -> param.activity4)
    val activityWithBonusNumber = paramMap.filter((activityNumber: String, activityName: String) => activityName == param.activityWithBonus).keys.head
    activityWithBonusNumber should be ("2")

    When("computing votes")
    val voteComputed = computeVotes(votes,paramMap, activityWithBonusNumber)

    Then("every activity should have right numbers of votes")

    voteComputed.size should be (4)
    val resultActivity2 = voteComputed("2")
    resultActivity2.counterVote should be(32) // with +1 bonus
    resultActivity2.nbVote1 should be(2)
    resultActivity2.nbVote2 should be(3)
    resultActivity2.nbVote3 should be(6)
    resultActivity2.nbVote4 should be(2)
    (resultActivity2.nbVote1 + resultActivity2.nbVote2 + resultActivity2.nbVote3 + resultActivity2.nbVote4) should be (votes.size)

  }

  "computeWinner" should "select the right winner according to votes" in {

    Given("a file with votes parsed")
    val source = Source.fromURL(getClass.getResource("/votes.txt"))
    val json = source.getLines().toSeq
    source.close()
    val votes: Seq[Vote] = parseVotes(json)
    And("parameters with activity name and bonus")
    val param = Parameter("marais salants de guérande", "brassage de binouze", "eventyr halloween", "fly a 75 boules", "brassage de binouze")
    val paramMap = Map("1" -> param.activity1, "2" -> param.activity2, "3" -> param.activity3, "4" -> param.activity4)
    val activityWithBonusNumber = paramMap.filter((activityNumber: String, activityName: String) => activityName == param.activityWithBonus).keys.head
    activityWithBonusNumber should be("2")
    And("vote results")
    val voteComputed = computeVotes(votes, paramMap, activityWithBonusNumber)

    When("computing winner")
    val winner = computeWinner(activityWithBonusNumber,voteComputed)

    Then("winner should be the activity with max counter and higher number of votes")

    winner._1 should be("3")


  }

}
