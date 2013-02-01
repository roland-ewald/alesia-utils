package alesia.utils.evaluation

/**
 * @author Roland Ewald
 */
trait ExperimentEntityPool[A] {

  val pool = scala.collection.mutable.Map[String, List[A]]()

  def domains = pool.keySet

  def add(domain: String, newEntities: A*) = {
    val existingProblems = pool.getOrElseUpdate(domain, List[A]())
    pool(domain) = newEntities.toList ++ existingProblems
  }

  def byDomain(domain: String):List[A] = pool.getOrElse(domain, List())

}