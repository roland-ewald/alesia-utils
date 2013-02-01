package alesia.utils.evaluation.expdata

import scala.collection.mutable.ListBuffer
import org.jamesii.SimSystem
import alesia.componentrating.misc.Serializer
import sessl.Simulator
import alesia.utils.evaluation.SimulationProblemPool
import alesia.utils.evaluation.ExperimentTestPool


/**
 * Interface to hide execution details.
 *
 * @author Jonathan Wienss
 * @author Roland Ewald
 *
 */
trait ExperimentExecutor {
  def getResults(problems: List[(String, Double, Map[String, Any])], teams: List[Simulator]): List[Double]
}

class CachedExperimentExecutor(val fileNames: String*) extends ExperimentExecutor {

  val problemPool = ExperimentTestPool

  val rng = SimSystem.getRNGGenerator().getNextRNG()

  val perfDatas = fileNames.map(f =>
    Serializer.fromFile(f).asInstanceOf[scala.collection.mutable.Map[(String, Simulator), ListBuffer[Double]]])

  //merge per-team data
  val mergedPerformanceData = (for (probTeamTuple <- perfDatas.head.keySet) yield {
    (probTeamTuple, (for (data <- perfDatas) yield data(probTeamTuple).toList).flatten.toIndexedSeq)
  }).toMap

  override def getResults(problems: List[(String, Double, Map[String, Any])], teams: List[Simulator]): List[Double] = {
    val executableExperiments = problemPool.problemsAndDefinitions(problems, teams)
    executableExperiments.map { setup =>
      val problemDef = setup._1.toString
      pickRandomly(mergedPerformanceData((problemDef, setup._2.simulator)))
    }
  }

  def pickRandomly[T](seq: IndexedSeq[T]): T = seq(rng.nextInt(seq.length))
}

class DefaultExperimentExecutor(val problemPool: SimulationProblemPool) extends ExperimentExecutor {
  override def getResults(problems: List[(String, Double, Map[String, Any])], teams: List[Simulator]): List[Double] = {
    val executableExperiments = problemPool.problemsAndDefinitions(problems, teams)
    executableExperiments.map { setup =>
      sessl.execute(setup._2)
      setup._2.runtime.get
    }
  }
} 