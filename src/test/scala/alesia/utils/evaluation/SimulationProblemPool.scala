package alesia.utils.evaluation

import sessl.Simulator

/**
 * Defines a pool of simulation problems.
 *
 * @author Roland Ewald
 */
class SimulationProblemPool extends ExperimentEntityPool[(String, Double, Map[String, Any])] {

  type ProblemDefinition = (String, Double)

  type Parameterization = Map[String, Any]

  type ParameterizedProblemDefinition = (String, Double, Parameterization)

  def addSimpleProblems(domain: String, problems: ProblemDefinition*) =
    add(domain, problems.map(p => (p._1, p._2, Map[String, Any]())): _*)

  def addParameterizedProblem(domain: String, problem: ProblemDefinition, params: Parameterization*) =
    add(domain, params.map(p => (problem._1, problem._2, p)): _*)

  def problems(domain: String): List[SingleExecExperiment] =
    problemDefinitions(domain).map(p => SingleExecExperiment(p._1, p._2, p._3))

  def problemDefinitions(domain: String): List[ParameterizedProblemDefinition] =
    pool.getOrElse(domain, List[ParameterizedProblemDefinition]())

  def problemDefinitions: List[ParameterizedProblemDefinition] = domains.toList.sorted.flatMap(problemDefinitions)

  def problems: List[SingleExecExperiment] = domains.toList.sorted.flatMap(problems)

  def problems(domain: String, simulators: List[Simulator]): List[SingleExecExperiment] =
    problems(problemDefinitions(domain), simulators)

  def problems(problems: List[ParameterizedProblemDefinition], simulators: List[Simulator]): List[SingleExecExperiment] =
    for (p <- problems; s <- simulators)
      yield new SingleExecExperiment(p._1, p._2, p._3) {
      simulator = s
    }

  def problemsAndDefinitions(problems: List[ParameterizedProblemDefinition], simulators: List[Simulator]): List[(ParameterizedProblemDefinition, SingleExecExperiment)] =
    for (p <- problems; s <- simulators)
      yield (p, new SingleExecExperiment(p._1, p._2, p._3) {
      simulator = s
    })

}

/** There is one simulation problem for each formalism (StoPi, SR, SRS, PDEVS). */
object SimpleTestPool extends SimulationProblemPool {
  addSimpleProblems("sr",
    ("java://examples.sr.CyclicChainSystem", .1))
  addSimpleProblems("srs",
    ("file-srs:///./models/SRSTest.srs", 10))
  addSimpleProblems("stopi",
    ("file-stopi:///./models/NaCl.stopi", .1))
  //TODO: Not available in official release:
  //  add("pdevs",
  //    ("java://examples.pdevs.forestfire2.ForestFire", Double.MaxValue, Map("vis_ascii" -> false)))
}

/** There are 20 simulation problems, 5 each for StoPi, SR, SRS, and PDEVS. */
object ExperimentTestPool extends SimulationProblemPool {
  addParameterizedProblem("sr", ("java://examples.sr.LinearChainSystem", 100),
    Map(),
    Map("numOfSpecies" -> 1000),
    Map("numOfInitialParticles" -> 2000))
  addSimpleProblems("sr",
    ("java://examples.sr.TotallyIndependentSystem", .05),
    ("java://examples.sr.CyclicChainSystem", 20))
  addSimpleProblems("srs",
    ("file-srs:///./models/SRSTest.srs", 1000),
    ("file-srs:///./models/PhosphoProtein_diffSpeed1d0_K100_Ph100_P1000_11x11x11.srs", 0.1),
    ("file-srs:///./models/OnlyDiffusionWellStirred3x3.srs", 1000),
    ("file-srs:///./models/OnlyDiffusionMix3x3.srs", 1000),
    ("file-srs:///./models/OnlyDiffusionCenter3x3.srs", 3000))
  addSimpleProblems("stopi",
    ("file-stopi:///./models/Bistable_Gene_Network.stopi", 20000),
    ("file-stopi:///./models/MgCl2.stopi", 1),
    ("file-stopi:///./models/Lotka_Volterra.stopi", 5),
    ("file-stopi:///./models/AutoregulatoryGeneticNetwork.stopi", 2000),
    ("file-stopi:///./models/Simple_Ligand_Receptor.stopi", 2000))
  addParameterizedProblem("pdevs", ("java://examples.pdevs.forestfire2.ForestFire", Double.MaxValue),
    List(
      50,
      60,
      70,
      80,
      100).map(x => Map("width" -> x, "height" -> x, "vis_ascii" -> false)): _*)
}