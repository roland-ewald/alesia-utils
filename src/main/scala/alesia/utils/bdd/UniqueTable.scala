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

import sessl.util.Logging
import scala.annotation.tailrec

/**
 * A very simple unique table implementation, roughly following the scheme described by D. E. Knuth in 'The Art of Computer Programming', vol. 4,
 *  fascicle 1, p. 92 et sqq.
 *
 *  @author Roland Ewald
 */
class UniqueTable extends Logging {

  //Use mutable data structures internally, for performance reasons
  import scala.collection.mutable._

  /** Current number of managed variables. */
  private[this] var numOfVariables = 0

  /** The counter to generate unique instruction ids. */
  private[this] var instrIdCounter = 0;

  /**
   * The storage for all instructions. For each variable with index v, the map contains a map from ([v],p,q) to r, where r is
   *  the id of the instruction.
   */
  private[this] val instructions = Map[Int, Map[(Int, Int), Int]]()

  /** Maps instruction IDs to their variables: r => v. Default values refer to false and true instruction. */
  private[this] val variables = Map[Int, Int]()

  /** Maps instruction IDs to their lower branch instructions: r => r_l. */
  private[this] val lowInstr = Map[Int, Int]()

  /** Maps instruction IDs to their higher branch instructions: r => r_h. */
  private[this] val highInstr = Map[Int, Int]()

  /** The false instruction id. Always first element in any array of branch instructions. */
  val falseInstrId = addNewInstruction(0, 0, 0)

  /** The true instruction. Always second element in any array of branch instructions. */
  val trueInstrId = addNewInstruction(0, 1, 1)

  /**
   * Look up unique node in table. See Knuth's TAOCP (see above), sec. 7.1.4, algorithm U.
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
        ensureVarAvailable(vNew)
      numOfVariables = varIdx + 1
    }

    //Find instruction 
    val instruction = instructions(varIdx).get(lowId, highId)
    if (instruction.isDefined)
      instruction.get
    else
      addNewInstruction(varIdx, lowId, highId)
  }

  /**
   * Adds a new instruction to the unique table.
   * @param varIdx the index of the variable
   * @param lowInstrIdx the index of the low instruction
   * @param highInstrIdx the index of the high instruction
   * @return the id of the new instruction
   */
  private[this] def addNewInstruction(varIdx: Int, lowInstrIdx: Int, highInstrIdx: Int): Int = {
    ensureVarAvailable(varIdx)

    val id = createNewId()
    variables(id) = varIdx
    lowInstr(id) = lowInstrIdx
    highInstr(id) = highInstrIdx
    instructions(varIdx)((lowInstrIdx, highInstrIdx)) = id
    id
  }

  /** Ensures the variable is available in the instruction cache. */
  private[this] def ensureVarAvailable(varIdx: Int): Unit = {
    val alreadyAvailable = instructions.contains(varIdx)
    if (!alreadyAvailable) {
      instructions(varIdx) = Map[(Int, Int), Int]()
    }
    alreadyAvailable
  }

  /**
   * Creates a new instruction id.
   *  @return the newly created id
   */
  private[this] def createNewId(): Int = {
    val rv = instrIdCounter
    instrIdCounter = instrIdCounter + 1
    rv
  }

  /**
   * Evaluates an instruction ID regarding an array of boolean values, indexed by the variable to which they refer.
   */
  @tailrec
  final def evaluate(instrId: Int, values: Array[Boolean]): Boolean = {
    require(variables.contains(instrId), "Instruction with id '" + instrId + "' is not available.")
    val low = lowInstr(instrId)
    val high = highInstr(instrId)
    //If this is one of the two fundamental instructions, return
    if (low == high)
      return low == 1
    else {
      val nextInstrId = if (values(variables(instrId) - 1)) low else high
      evaluate(nextInstrId, values)
    }
  }

  /**
   * Combines two functions via 'and'.
   * @param f the instruction id of the first function
   * @param g the instruction id of the second function
   */
  def and(f: Int, g: Int): Int = {
    require(f < instrIdCounter && g < instrIdCounter && f >= 0 && g >= 0, "Instruction IDs must be valid.")

    def constructInstructions(id: Int, varIdx: Int, minVarIdx: Int): (Int, Int) = {
      if (varIdx == minVarIdx)
        (lowInstr(id), highInstr(id))
      else
        (id, id)
    }

    //'and' is commutative, so make this pair unique via ordering 
    val (id1, id2) = if (f < g) (f, g) else (g, f)

    //Check for obvious solutions
    if (id1 == id2 || id2 == 1)
      return id1
    if (id1 == 1)
      return id2
    if (id1 == 0 || id2 == 0)
      return 0

    //'Melding' both function, see eq. 7.1.4.-(52) 
    val (var1Idx, var2Idx) = (variables(id1), variables(id2))
    val minVarIdx = math.min(var1Idx, var2Idx)
    val (id1LowInstr, id1HighInstr) = constructInstructions(id1, var1Idx, minVarIdx)
    val (id2LowInstr, id2HighInstr) = constructInstructions(id2, var2Idx, minVarIdx)

    //Recursively construct new function
    val lowInstrResult = and(id1LowInstr, id2LowInstr)
    val highInstrResult = and(id1HighInstr, id2HighInstr)

    val rv = unique(minVarIdx, lowInstrResult, highInstrResult)
    //TODO: cache result

    rv
  }

  //TODO: Add methods for composition/reduction, ordering

}