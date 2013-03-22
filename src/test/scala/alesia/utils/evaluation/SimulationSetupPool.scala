package alesia.utils.evaluation

import sessl._
import sessl.james._
import sessl.util.CreatableFromVariables

/**
 * A 'pool' of simulation setups, divided by domain.
 *
 * @author Roland Ewald
 *
 */
class SimulationSetupPool extends ExperimentEntityPool[Simulator] {

  val playersPerTeam = scala.collection.mutable.Map[Simulator, Set[String]]()

  val teamsPerPlayer = scala.collection.mutable.Map[String, Set[Simulator]]()

  override def add(domain: String, newEntities: Simulator*) =
    newEntities.foreach(e => addSimulator(domain, e, Set(e.toString)))

  def addSimulator(domain: String, sim: Simulator, players: Set[String]) = {
    super.add(domain, sim)
    require(!playersPerTeam.contains(sim), "Simulator setup '" + sim + "' must not be added twice.")
    playersPerTeam(sim) = players
    players.foreach { player =>
      val oldTeams = teamsPerPlayer.getOrElse(player, Set())
      teamsPerPlayer(player) = oldTeams + sim
    }
  }

  def addSimulators(domain: String, simTeams: (Simulator, Set[String])*) =
    simTeams.foreach(s => addSimulator(domain, s._1, s._2))

  def isPartOfTeam(id: String, sim: Simulator) = playersPerTeam.getOrElse(sim, Set())(id)

  def players = teamsPerPlayer.keySet.toList.sorted

}

object SimulationSetupPool {

  val eventQueues = generatePlayerNames(List(
    SortedList(),
    BucketQueue(),
    Heap(),
    LinkedList() //Not yet released for JAMES II:
    //    CalendarQueue(),
    //    CalendarReQueue(),
    //    DynamicCalendarQueue(),
    //    MList(),
    //    MedianPointer(),
    //    TwoList()
    ))

  val srSimulators = combinePlayersToTeams(List(
    NextReactionMethod() /*, Not yet released for JAMES II:
    NextReactionMethodB(),
    NextReactionEventMethod()*/ ), "eventQueue", false)

  val srsSimulators = combinePlayersToTeams(List(
    NextSubvolumeMethod()), "EventQueue")

  val stopiSimulators = combinePlayersToTeams(List(
    PopulationFocusedSimulator(),
    ChannelFocusedSimulator(),
    CommunicationFocusedSimulator()), "EventQueue")

  val pdevsSimulators = combinePlayersToTeams(List(
    PDEVSFlatSequential()), "eventqueue")

  def combinePlayersToTeams[A <: JamesIIAlgo[_] with CreatableFromVariables[_]](simulators: List[A],
    eqProperty: String, chooseString: Boolean = true) = {
    for (sim <- generatePlayerNames(simulators); eq <- eventQueues)
      yield (
      //TODO: Refactor to avoid cast:
      (sim._1 scan (eqProperty <~ (if (chooseString) eq._2 else eq._1))).head.asInstanceOf[Simulator],
      Set(sim._2, eq._2))
  }

  def generatePlayerNames[U <: JamesIIAlgo[_]](algos: List[U]) =
    algos.map(a => (a, a.factory.getClass().getCanonicalName()))
}

object FullSimulationSetupPool extends SimulationSetupPool {

  import SimulationSetupPool._

  addSimulators("sr", srSimulators: _*)
  addSimulators("srs", srsSimulators: _*)
  addSimulators("stopi", stopiSimulators: _*)
  addSimulators("pdevs", pdevsSimulators: _*)

}

