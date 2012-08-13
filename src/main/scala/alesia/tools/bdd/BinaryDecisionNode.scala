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

/** Branch instruction to represent a node in a BDD. */
sealed trait BranchInstruction {
  val variable: Int
  val lowIndex: Int
  val highIndex: Int
}

/** A common branch instruction. */
case class BranchInstr(val variable: Int, val lowIndex: Int, val highIndex: Int) extends BranchInstruction

/** Branch instruction to represent the TrueNode.
 *  @param variable non-existing variable index (n+1, with n being the maximal index)
 */
case class TrueNodeInstruction(val variable: Int) extends BranchInstruction {
  val lowIndex = 1
  val highIndex = 1
}

/** Branch instruction to represent the FalseNode.
 *  @param variable non-existing variable index (n+1, with n being the maximal index)
 */
case class FalseNodeInstruction(val variable: Int) extends BranchInstruction {
  val lowIndex = 0
  val highIndex = 0
}

/** Conversion from/to a set of branch instructions. */
object BinaryDecisionNode {

  /** Convert binary decision diagram to array of branch instructions. */
  implicit def asInstructions(node: BinaryDecisionNode): Array[BranchInstruction] = {

    //Basic map to store the instructions for each node, I_0 and I_1 are added by default
    val nodeMap = scala.collection.mutable.Map[BinaryDecisionNode, (Int, BranchInstruction)](
      FalseNode -> (0, FalseNodeInstruction(-1)), TrueNode -> (1, TrueNodeInstruction(-1)))

    //Stores the largest variable index that has been encountered
    var maxVariableIndex = 0

    /** Fills the instruction map with the entry for the given BDD node.
     *  @return the index of the instruction
     */
    def fillNodeMap(node: BinaryDecisionNode): Int = node match {
      case node: BDDNode => {
        val nodeEntry = nodeMap.get(node)
        maxVariableIndex = scala.math.max(maxVariableIndex, node.variable)
        if (nodeEntry.isDefined) nodeEntry.get._1
        else {
          val lowNodeIdx = fillNodeMap(node.low)
          val highNodeIdx = fillNodeMap(node.high)
          val nodeIdx = nodeMap.size
          nodeMap += (node -> (nodeIdx, BranchInstr(node.variable, lowNodeIdx, highNodeIdx)))
          nodeIdx
        }
      }
      case FalseNode => 0
      case TrueNode => 1
    }

    fillNodeMap(node)

    //Update instructions representing the constant functions with variable index n+1
    val varIdxConstants = maxVariableIndex + 1
    nodeMap(FalseNode) = (0, FalseNodeInstruction(varIdxConstants))
    nodeMap(TrueNode) = (1, TrueNodeInstruction(varIdxConstants))

    nodeMap.toList.sortBy(_._2._1).map(_._2._2).toArray
  }

  /** Convert array of branch instructions to binary decision diagram. */
  implicit def asBinaryDecisionNode(instructions: Array[BranchInstruction]): BinaryDecisionNode = {
    val nodeMap = scala.collection.mutable.Map[Int, BinaryDecisionNode](0 -> FalseNode, 1 -> TrueNode)
    instructions.zipWithIndex.foreach(tuple =>
      {
        val (inst, idx) = tuple
        if (!nodeMap.contains(idx)) {
          nodeMap += (idx -> BDDNode(inst.variable, nodeMap(inst.lowIndex), nodeMap(inst.highIndex)))
        }
      })
    nodeMap(instructions.size - 1)
  }

}