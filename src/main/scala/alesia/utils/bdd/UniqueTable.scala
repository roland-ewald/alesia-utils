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
package alesia.utils.bdd

/** A very simple unique table implementation, roughly following the scheme described by D. E. Knuth in 'The Art of Computer Programming', vol. 4,
 *  fascicle 1, p. 92 et sqq.
 *
 *  @author Roland Ewald
 */
class UniqueTable {

  //Use mutable data structures internally, for performance reasons
  import scala.collection.mutable._

  /** Current number of managed variables. */
  private[this] var numOfVariables = 0

  /** The counter starts at 2 since by definition falseInstr has index 0 and trueInstr has index 1. */
  private[this] var instrIdCounter = 2;

  /** The false instruction. Always first element in any array of branch instructions. */
  private[this] def falseInstr = BranchInstr(numOfVariables, 0, 0)

  /** The true instruction. Always second element in any array of branch instructions. */
  private[this] def trueInstr = BranchInstr(numOfVariables, 1, 1)

  /** The storage for all instructions. For each variable with index v, the map contains a map from ([v],p,q) to r, where r is
   *  the id of the instruction.
   */
  private[this] val instructions = Map[Int, Map[(Int, Int), Int]]()

  /** Maps instruction IDs to their variables: r => v. */
  private[this] val variables = Map[Int, Int]()

  /** Maps instruction IDs to their lower branch instructions: r => r_l. */
  private[this] val lowInstr = Map[Int, Int]()

  /** Maps instruction IDs to their higher branch instructions: r => r_h. */
  private[this] val highInstr = Map[Int, Int]()

  /** Look up unique node in table. See Knuth's TAOCP (see above), sec. 7.1.4, algorithm U.
   *  @param varIdx the index of the variable the node relates to
   *  @param lowId the id of the low-branch instruction (to be followed if x_v is false)
   *  @param highId the id of the high-branch instruction (to be followed if x_v is true)
   *  @return the id of the branch instruction
   */
  def unique(varIdx: Int, lowId: Int, highId: Int): Int = {

    //If the outcome does not depend on the variable, simply return the id of the (identical) branch instruction
    if (lowId == highId)
      return lowId

    //Check whether there is a new variable to be considered
    if (numOfVariables <= varIdx) {
      for (vNew <- numOfVariables to varIdx)
        instructions(vNew) = Map[(Int, Int), Int]()
      numOfVariables = varIdx + 1
    }

    //Find instruction 
    val instruction = instructions(varIdx).get(lowId, highId)
    if (instruction.isDefined)
      return instruction.get

    //Create new instruction id
    val newId = createNewId()
    variables(newId) = varIdx
    lowInstr(newId) = lowId
    highInstr(newId) = highId
    instructions(varIdx)((lowId, highId)) = newId
    newId
  }

  /** Creates a new instruction id.
   *  @return the newly created id
   */
  private[this] def createNewId(): Int = {
    val rv = instrIdCounter
    instrIdCounter = instrIdCounter + 1
    rv
  }
  
  //TODO: Add methods for composition/reduction, ordering, evaluation...

}