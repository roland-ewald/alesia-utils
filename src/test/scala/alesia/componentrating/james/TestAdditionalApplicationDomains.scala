package alesia.componentrating.james

import java.util.logging.Level
import org.jamesii.core.util.logging.ApplicationLogger
import org.junit.runner.RunWith
import org.junit.runner.RunWith
import org.scalatest.FunSpec
import org.scalatest.junit.JUnitRunner
import org.scalatest.junit.JUnitRunner
import alesia.utils.evaluation.FullSimulationSetupPool
import alesia.utils.evaluation.SimpleTestPool
import sessl._
import sessl.james._
import sessl.james.Experiment
import sessl.james.PerformanceObservation
import sessl.util.Logging
import alesia.utils.evaluation.SingleExecExperiment

/**
 * @author Roland Ewald
 */
@RunWith(classOf[JUnitRunner])
class TestAdditionalApplicationDomains extends FunSpec with Logging {

  ApplicationLogger.setLogLevel(Level.SEVERE);

  val problemPool = SimpleTestPool

  val setupPool = FullSimulationSetupPool

  describe("TrueSkill rating system + SESSL ") {
    List("sr", "srs", "stopi", "pdevs").foreach { domain =>
      it("can be applied to '" + domain + "'") {
        executeAndCheck(problemPool.problems(domain, setupPool.byDomain(domain)))
      }
    }
  }

  def executeAndCheck(experiments: List[SingleExecExperiment]) = {
    experiments.par.foreach(exp => {
      logger.info("Starting experiment with '" + exp.simulator + "' on '" + exp.modelToUse + "'...")
      sessl.execute(exp)
    })
    assert(experiments.forall(e => e.runtime.isDefined && e.runtime.get > .0))
  }

}