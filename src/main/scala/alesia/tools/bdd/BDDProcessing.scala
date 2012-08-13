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
  def evaluate(instructions: Array[BranchInstruction], input: Array[Boolean]): Boolean = {
    @tailrec
    def evaluate(instructions: Array[BranchInstruction], input: Array[Boolean], currentIndex: Int): Boolean = instructions(currentIndex) match {
      case i: BranchInstr => if (input(i.variable)) evaluate(instructions, input, i.highIndex) else evaluate(instructions, input, i.lowIndex)
      case t: TrueNodeInstruction => true
      case f: FalseNodeInstruction => false
    }
    evaluate(instructions, input, instructions.size - 1)
  }

  /** Count BDD solutions (input variable combinations for which f(x_1, ..., x_n) = true).
   *  Corresponds to algorithm 7.1.4.C (p. 75, TAOCP - see above).
   */
  def countSolutions(bdd: Array[BranchInstruction]): Int = {
    checkBDDDimension(bdd.size)

    //Contains number of solutions (ones) for the bead corresponding to the branch instruction with the same index
    val counter = new Array[Int](bdd.size)

    //Computes the number of combinations between the index of a node and the given node with variable index v_k
    def combinations(idx: Int, v_k: Int) = counter(idx) * (1 << (bdd(idx).variable - v_k - 1))

    //C1
    counter(1) = 1
    for (k <- 2 until bdd.size) {
      val v = bdd(k).variable
      counter(k) = combinations(bdd(k).lowIndex, v) + combinations(bdd(k).highIndex, v) //C2
    }

    combinations(bdd.size - 1, -1) // Second arg needs to be -1 because variable indices start with 0
  }

  /** Reduce BDD if necessary. It is assumed the BDD is already ordered,
   *  i.e. children of a node related to v_i may only be related to variables v_{i+1}, ..., v_n.
   */
  def reduce(bdd: Array[BranchInstruction]): BinaryDecisionNode = {
    checkBDDDimension(bdd.size)
    val rootIdx = bdd.size - 1
    val aux = new Array[Int](bdd.size)
    val head = new Array[Int](bdd(0).variable - 1) // false & true node have variable index v_{n+1}

    FalseNode
  }

  /** Checks dimension of BDD.*/
  private[this] def checkBDDDimension(size: Int) =
    require(size > 1, "A valid BDD consists of at least two branch instructions, but there are only " + size + " present.")
}