package alesia.componentrating.james

import scala.util.Random
import org.junit.runner.RunWith
import org.junit.runner.RunWith
import org.scalatest.FunSpec
import org.scalatest.junit.JUnitRunner
import org.scalatest.junit.JUnitRunner
import alesia.componentrating.activeRanking.ActiveRanking
import alesia.componentrating.activeRanking.MaxRoundStopCondition
import alesia.componentrating.activeRanking.Round
import alesia.componentrating.activeRanking.FileRatingLogger
import alesia.componentrating.activeRanking.misc.HammingDistance
import alesia.componentrating.activeRanking.misc.NumberOfInversionsDistance
import alesia.componentrating.activeRanking.misc.PointsAndUncertaintyMatchQuality
import alesia.componentrating.activeRanking.misc.WeightedRandomMatchUpSelector
import alesia.componentrating.misc.AdvancedOptions
import alesia.componentrating.misc.Serializer
import alesia.componentrating.misc.TrueSkillDefaultValues
import junit.framework.Assert._
import sessl.util.Logging
import java.io.File
import alesia.componentrating.TrueSkillRatingSystem

/**
 * Quick test showing how to use active ranking.
 * Before executing this test:
 * 			* need one file named realRankingFile.xml in working directory
 * 					(for calculating the distance/progress)
 * 					(execute CreateRealRankingFile.scala and copy the created file)
 * 			* needs team performance files for the JamesIIComparator in working directory
 * 					(alternatively, implement your own TSComparator to draw the component
 * 						performances from a different source)
 * 			* creates files containing the results of the experiment.
 *
 * @author Jonathan Wienss
 *
 */
@RunWith(classOf[JUnitRunner])
class TestActiveRanking extends FunSpec with Logging {

  val dflt = new TrueSkillDefaultValues
  val advOpt = new AdvancedOptions // default TrueSkill options, like team size balancing
  val seed: java.lang.Long = 2
  val rng = new Random
  val partialPlayRanking = "NewActiveRankingResults_PartialPlay.xml"
  val virtualPlayersRanking = "NewActiveRankingResults_VirtualPlayers.xml"

  startRun(rng, dflt, advOpt.cloneAndSet(setPPBalancing = true), partialPlayRanking)
  startRun(rng, dflt, advOpt.cloneAndSet(setVPBalancing = true), virtualPlayersRanking)

  describe("Active Ranking Test") {

    it("runs smoothly and saves results to file") {
      assert(new File(partialPlayRanking).exists())
      assert(new File(virtualPlayersRanking).exists())
    }

    test()
  }

  def startRun(rng: Random, dflt: TrueSkillDefaultValues, advOpt: AdvancedOptions, targetFile: String) {
    val muSelector = new WeightedRandomMatchUpSelector(new PointsAndUncertaintyMatchQuality[sessl.Simulator]()(dflt, advOpt))
    val comparator = new JamesIIComparator(muSelector, 2)
    val stopCondition = new MaxRoundStopCondition(stopRound = 30000, maxReplications = 10)
    val logger = new FileRatingLogger(targetFile, "./componentrating/realRankingFile.xml", List(new HammingDistance, new NumberOfInversionsDistance))
    val ar = new ActiveRanking(new TrueSkillRatingSystem, dflt, advOpt, rng, stopCondition, comparator, logger)
    ar.execute
  }

  /* --- Code for JUnit tests below here --- */

  def test() {
    var pp: Round = null
    var vp: Round = null
    pp = Serializer.fromFile("NewActiveRankingResults_PartialPlay.xml")
    vp = Serializer.fromFile("NewActiveRankingResults_VirtualPlayers.xml")
    it("reads results back in") {
      assertNotNull(pp)
      assertNotNull(vp)
    }

    def foo(time: Int, distance: Int, r: Round) = recursiveMeanE(r.info.keySet.toList, 0.0, add, div, (x: Int) => pp.info.get(x).get(time).values.toList(distance).toDouble)
    it("first Distance, PP: the Distance is smaller in the end then in the beginning") {
      val time1 = foo(0, 0, pp)
      val time2 = foo(9990, 0, pp)
      System.out.println(time1 + " " + time2)
      assertTrue("Failed: distance at time=0 and time=1000: " + time1 + time2, time1 > time2)
    }
    it("second Distance, PP: the Distance is smaller in the end then in the beginning") {
      val time1 = foo(0, 1, pp)
      val time2 = foo(9990, 1, pp)
      System.out.println(time1 + " " + time2)
      assertTrue("Failed: distance at time=0 and time=1000: " + time1 + time2, time1 > time2)
    }
    it("first Distance, VP: the Distance is smaller in the end then in the beginning") {
      val time1 = foo(0, 0, vp)
      val time2 = foo(9990, 0, vp)
      System.out.println(time1 + " " + time2)
      assertTrue("Failed: distance at time=0 and time=1000: " + time1 + time2, time1 > time2)
    }
    it("second Distance, VP: the Distance is smaller in the end then in the beginning") {
      val time1 = foo(0, 1, vp)
      val time2 = foo(9990, 1, vp)
      System.out.println(time1 + " " + time2)
      assertTrue("Failed: distance at time=0 and time=1000: " + time1 + time2, time1 > time2)
    }
  }

  def recursiveMeanE[T, P](l: List[P], nul: T, sum: (T, T) => T, finaly: (T, Int) => T, extract: P => T): T = {
    var result: T = nul
    l.foreach(x => result = sum(result, extract(x)))
    finaly(result, l.size)
  }
  def add(a: Double, b: Double): Double = a + b
  def div(a: Double, b: Int): Double = a / b
}