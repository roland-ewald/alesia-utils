/*
 * Copyright 2012 Roland Ewald
 *  
 * Licensed under the Apache License, Version 2.0 (the "License"); 
 * you may not use this file except in compliance with the License. 
 * You may obtain a copy of the License at 
 *  
 *  http://www.apache.org/licenses/LICENSE-2.0
 *  
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied. 
 * See the License for the specific language governing permissions and 
 * limitations under the License. 
 */
package alesia.tools.bdd

import scala.annotation.tailrec

/** Basic types to represent a binary decision diagrams (BDDs).
 *  Correspond to entities discussed in D. E. Knuth's "The Art of Computer Programming", vol. 4,
 *  fascicle 1 (sec. 7.1.4, p. 70 et sqq.).
 */

/** Node in a binary decision diagram.*/
sealed trait BinaryDecisionNode {
  /** The index of the corresponding variable to check.*/
  val variable: Int
  /** The branch to follow in case the corresponding variable is set to false.*/
  val low: BinaryDecisionNode
  /** The branch to follow in case the corresponding variable is set to true.*/
  val high: BinaryDecisionNode
}

/** Non-leaf node. */
case class BDDNode(val variable: Int, val low: BinaryDecisionNode, val high: BinaryDecisionNode) extends BinaryDecisionNode

/** Leaf, represents the constant 'true'. */
object TrueNode extends BinaryDecisionNode {
  val variable = -1
  val low = TrueNode
  val high = TrueNode
}

/** Leaf, represents the constant 'false'. */
object FalseNode extends BinaryDecisionNode {
  val variable = -1
  val low = FalseNode
  val high = FalseNode
}

/** Instruction to represent a node in a BDD. */
sealed case class Instruction(val variable: Int, val lIndex: Int, val rIndex: Int)

/** Helper methods. */
object BinaryDecisionNode {
  def asInstructions(node: BinaryDecisionNode): Array[Instruction] = {
    null
  }
  
  /** Evaluate node for given input. */
  @tailrec
  def evaluate(node: BinaryDecisionNode, input: Array[Boolean]): Boolean = node match {
    case TrueNode => return true
    case FalseNode => return false
    case node: BDDNode => if (input(node.variable)) evaluate(node.high, input) else evaluate(node.low, input)
  }
}