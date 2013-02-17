package alesia.componentrating.james

import alesia.componentrating.activeRanking.misc.MatchUpSelector
import alesia.componentrating.activeRanking.ActiveRanking
import scala.collection.mutable.HashMap
import alesia.componentrating.misc.Helper
import alesia.componentrating.activeRanking.RankingComparator
import alesia.utils.evaluation.expdata.CachedExperimentExecutor
import alesia.utils.evaluation.ExperimentTestPool
import alesia.utils.evaluation.FullSimulationSetupPool

/**
 * JamesIIComparator compares Teams based on their performance in JamesII experiments.
 *
 * @author Jonathan Wienss
 *
 */
class JamesIIComparator(muSelector: MatchUpSelector[sessl.Simulator], teamsPerGame: Int, perfDataDirectory: String = "./componentrating/")
  extends RankingComparator() {

  private[this] var aR: ActiveRanking = null
  def register(aR: ActiveRanking) = this.aR = aR

  val executor = new CachedExperimentExecutor((1 to 4).map(perfDataDirectory + "teamPerformances" + _ + ".xml"): _*)
  val problemPool = ExperimentTestPool
  val setupPool = FullSimulationSetupPool
  override def getComponents: scala.collection.immutable.Set[String] = {
    val x = ExperimentTestPool.domains.flatMap(FullSimulationSetupPool.byDomain).flatMap(FullSimulationSetupPool.playersPerTeam)
    return (x.toList.toSet)
  }

  val biasedDomains = {
    val list = {
      var temp = List[String]() // temporary!
      (problemPool.domains zip problemPool.domains.map(setupPool.byDomain)).foreach(x => x._2.foreach(v1 => temp = x._1 :: x._1 :: temp)) // for each simulator...
      temp
    }
    var temp = List[String]()
    (list zip list.map(problemPool.byDomain)).foreach(x => x._2.foreach(v1 => temp = x._1 :: x._1 :: temp)) // for each problem...
    temp
  }

  def apply() = {
    val domain = Helper.chooseRand(biasedDomains, aR.rng)
    val problem = Helper.chooseRand(problemPool.byDomain(domain), aR.rng)

    muSelector.matchQuality.init(setupPool.byDomain(domain), setupPool.playersPerTeam.toSeq.map(tuple => (tuple._1, tuple._2.toList)).toMap, aR.crs)
    muSelector.init(setupPool.byDomain(domain), teamsPerGame, aR.rng)
    val teams = muSelector.getMatchUp
    // Log JAMESII experiment results:
    var tResult = executor.getResults(List(problem), teams)
    var teamPlayers = teams.map(setupPool.playersPerTeam)
    val teamTimes = HashMap[Set[String], Double]()
    teamPlayers.foreach(team => { teamTimes += team -> tResult.head; tResult = tResult.tail })

    teamPlayers.sortBy(teamTimes)
  }
}