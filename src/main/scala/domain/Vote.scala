package domain


case class Vote(vote1: String, vote2: String, vote3: String, vote4: String, name: String)

case class VoteResult(counterVote: Int =0 , nbVote1: Int =0, nbVote2: Int=0, nbVote3: Int=0, nbVote4: Int=0)

