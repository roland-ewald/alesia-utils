package alesia.componentrating.james
import sessl._NextReactionMethod
import sessl.james.BasicJamesIIEventQueue
import sessl.james.BasicJamesIISimulator
import sessl.james.JamesIIAlgo
import sessl.util.CreatableFromVariables
import simulator.pdevsflatsequential.FlatSequentialProcessorFactory
import simulator.pi.channelfocused.ChannelFocusedProcessorFactory
import simulator.pi.comfocused.CommunicationFocusedProcessorFactory
import simulator.pi.popfocused.PopulationFocusedProcessorFactory
import simulator.pi.reaction.plugintype.ReactionFactory
import simulator.srs.ssa.nsm.NSMProcessorFactory
import simulator.stopi.reaction.next.NextReactionMethodFactory
import sessl.james.Heap

/**
 * This class contains (non-functional) SESSL declaration for JAMES II algorithms that have not yet been released.
 *
 * @author Roland Ewald
 */
class UnreleasedJamesAlgorithmStubs

//SR
case class NextReactionMethodB(val eventQueue: BasicJamesIIEventQueue = Heap())
  extends CreatableFromVariables[NextReactionMethodB] with BasicJamesIISimulator {
  override def factory = null
}

case class NextReactionEventMethod(val eventQueue: BasicJamesIIEventQueue = Heap())
  extends CreatableFromVariables[NextReactionEventMethod] with _NextReactionMethod with BasicJamesIISimulator {
  override def factory = null
}

//SRS

case class NextSubvolumeMethod(val EventQueue: String = Heap().factory.getClass().getCanonicalName())
  extends CreatableFromVariables[NextSubvolumeMethod] with BasicJamesIISimulator {
  override def factory = new NSMProcessorFactory
}

//StoPi

case class NextReactionMethodType() extends JamesIIAlgo[ReactionFactory] {
  override def factory = new NextReactionMethodFactory
}

case class PopulationFocusedSimulator(val EventQueue: String = Heap().factory.getClass().getCanonicalName(),
  val ReactionType: String = NextReactionMethodType().factory.getClass().getCanonicalName())
  extends CreatableFromVariables[PopulationFocusedSimulator] with BasicJamesIISimulator {
  override def factory = new PopulationFocusedProcessorFactory
}

case class CommunicationFocusedSimulator(val EventQueue: String = Heap().factory.getClass().getCanonicalName(),
  val ReactionType: String = NextReactionMethodType().factory.getClass().getCanonicalName())
  extends CreatableFromVariables[CommunicationFocusedSimulator] with BasicJamesIISimulator {
  override def factory = new CommunicationFocusedProcessorFactory
}

case class ChannelFocusedSimulator(val EventQueue: String = Heap().factory.getClass().getCanonicalName(),
  val ReactionType: String = NextReactionMethodType().factory.getClass().getCanonicalName())
  extends CreatableFromVariables[ChannelFocusedSimulator] with BasicJamesIISimulator {
  override def factory = new ChannelFocusedProcessorFactory
}

//PDEVS

case class PDEVSFlatSequential(val eventqueue: String = Heap().factory.getClass().getCanonicalName())
  extends CreatableFromVariables[PDEVSFlatSequential] with BasicJamesIISimulator {
  override def factory = new FlatSequentialProcessorFactory
}

//Event Queues

case class CalendarQueue() extends BasicJamesIIEventQueue {
  override def factory = null
}

case class CalendarReQueue() extends BasicJamesIIEventQueue {
  override def factory = null
}

case class DynamicCalendarQueue() extends BasicJamesIIEventQueue {
  override def factory = null
}

case class DynamicCalendarQueueWithHashMap() extends BasicJamesIIEventQueue {
  override def factory = null
}

case class LadderQueueSingle() extends BasicJamesIIEventQueue {
  override def factory = null
}

case class MList() extends BasicJamesIIEventQueue {
  override def factory = null
}

case class MListRe() extends BasicJamesIIEventQueue {
  override def factory = null
}

case class DSplay() extends BasicJamesIIEventQueue {
  override def factory = null
}

case class MedianPointer() extends BasicJamesIIEventQueue {
  override def factory = null
}

case class TwoList() extends BasicJamesIIEventQueue {
  override def factory = null
}
