package alesia.utils.evaluation

import sessl._
import sessl.james._

/**
 * A runtime experiment.
 * @author Roland Ewald
 */
case class SingleExecExperiment(modelToUse: String, stopTimeToUse: Double, fixedParams: Map[String, Any]) extends Experiment with PerformanceObservation {
  private[this] var runtimes = Seq[Double]()
  model = modelToUse
  stopCondition = AfterSimTime(stopTimeToUse) or AfterWallClockTime(seconds = 60)
  fixedParams.foreach(param => set(param._1 <~ param._2))
  withExperimentPerformance { r =>
    {
      logger.info("Registering run time of '" + simulator + "' on '" + model + "' :" + r.runtimesFor(simulator).head._2)
      runtimes = r.runtimesFor(simulator).head._2
    }
  }
  def runtime = runtimes.headOption
}