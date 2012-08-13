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

/** Implementation of algorithms for managing, processing, and creating binary decision diagrams.
 *
 *  Most implementations are from D. E. Knuth's "The Art of Computer Programming", vol. 4-1 (p. 70 et sqq.).
 *
 *  @see BinaryDecisionNode
 *
 *  @author Roland Ewald
 */
object BDDProcessing {

  /** Evaluate node for given input. */
  @tailrec
  def evaluate(node: BinaryDecisionNode, input: Array[Boolean]): Boolean = node match {
    case node: BDDNode => if (input(node.variable)) evaluate(node.high, input) else evaluate(node.low, input)
    case TrueNode => return true
    case FalseNode => return false
  }

  /** Evaluate an instruction array for a given input. */
  def evaluate(instructions: Array[BranchInstruction], input: Array[Boolean]): Boolean = evaluate(instructions, input, instructions.size - 1)

  @tailrec
  private[this] def evaluate(instructions: Array[BranchInstruction], input: Array[Boolean], currentIndex: Int): Boolean = {
    val inst = instructions(currentIndex)
    inst.variable match {
      case TrueNode.variable => true
      case FalseNode.variable => false
      case x => if (input(x)) evaluate(instructions, input, inst.highIndex) else evaluate(instructions, input, inst.lowIndex)
    }
  }

  /** Count BDD solutions (input variable combinations for which f(x_1, ..., x_n) = true).
   *  Corresponds to algorithm 7.1.4.C (p. 75, TAOCP - see above).
   */
  def countSolutions(bdd: Array[BranchInstruction]): Int = {
    val counter = Array[Int](bdd.size)
    0
  }
}