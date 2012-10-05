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

  /** Maps function descriptions of the form f_{instr_id} + "AND" + g_{instr_id} onto instruction ids that represent these functions. */
  private[this] val solutionCache = Map[String, Int]()

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
    if (numOfVariables < varIdx) {
      for (vNew <- numOfVariables to varIdx)
        ensureVarAvailable(vNew)
      numOfVariables = varIdx
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

  /** @return number of defined variables */
  def variableCount = numOfVariables

  /** @return number of instructions */
  def instructionCount = instrIdCounter

  /**
   * Evaluates an instruction ID regarding an array of boolean values, indexed by the variable to which they refer.
   */
  @tailrec
  final def evaluate(f: Int, values: Array[Boolean]): Boolean = {
    requireInstrIds(f)

    val low = lowInstr(f)
    val high = highInstr(f)
    //If this is one of the two fundamental instructions, return
    if (low == high)
      return low == 1
    else {
      val nextInstrId = if (values(variables(f) - 1)) low else high
      evaluate(nextInstrId, values)
    }
  }

  // Logical operations

  /**
   * Combines two functions via 'and'.
   * @param f the instruction id of the first function
   * @param g the instruction id of the second function
   * @return the instruction id of the function "f∧g"
   */
  def and(f: Int, g: Int): Int = compose("∧", f, g, commutative = true) {
    (f1, f2) =>
      {
        if (f1 == f2 || f2 == 1)
          Some(f1)
        else if (f1 == 1)
          Some(f2)
        else if (f1 == 0 || f2 == 0)
          Some(0)
        else None
      }
  }

  /**
   * Combines two functions via 'or'.
   * @param f the instruction id of the first function
   * @param g the instruction id of the second function
   * @return the instruction id of the function "f∨g"
   */
  def or(f: Int, g: Int): Int = compose("∨", f, g, commutative = true) {
    (f1, f2) =>
      {
        if (f1 == f2 || f2 == 0)
          Some(f1)
        else if (f1 == 0)
          Some(f2)
        else if (f1 == 1 || f2 == 1)
          Some(1)
        else None
      }
  }

  /**
   * Combines two functions via 'xor'.
   * @param f the instruction id of the first function
   * @param g the instruction id of the second function
   * @return the instruction id of the function "f⊕g"
   */
  def xor(f: Int, g: Int): Int = compose("⊕", f, g, commutative = true) {
    (f1, f2) =>
      {
        if (f1 == 0)
          Some(f2)
        else if (f2 == 0)
          Some(f1)
        else if (f1 == f2 || (f1 == 1 && f2 == 1))
          Some(0)
        else None
      }
  }

  /**
   * Negates a function.
   * @param f the instruction id of the function to be negated
   * @return the instruction id of the function "¬f"
   */
  def not(f: Int): Int = xor(1, f)

  /**
   * Defines an implication f => g. This is rewritten to "(¬f)∨g".
   * @param f the condition
   * @param g the implication
   * @return the instruction id of the function "(¬f)∨g"
   */
  def implies(f: Int, g: Int) = or(not(f), g)

  // Operations on sets

  /** @return the instruction id that corresponds to the characteristic function of the empty set */
  def emptySet = 0

  /** @return true if this is the characteristic function of the empty set*/
  def isEmpty(f: Int) = (f == 0)

  /** @return the instruction id that corresponds to the characteristics function of the set that contains all elements (finitely many, all possible assignments for the known variables) */
  def fullSet = 1

  /**
   * Creates a characteristic function for the union S_f ∪ S_g, where each set is given by
   * its characteristic function (f and g, respectively).
   * @param f the characteristic function of S_f
   * @param g the characteristic function of S_g
   * @return the characteristic function of S_f ∪ S_g
   */
  def union(f: Int, g: Int): Int = or(f, g)

  /**
   * Creates a characteristic function for the intersection S_f ∩ S_g, where each set is given by
   * its characteristic function (f and g, respectively).
   * @param f the characteristic function of S_f
   * @param g the characteristic function of S_g
   * @return the characteristic function of S_f ∩ S_g
   */
  def intersection(f: Int, g: Int): Int = and(f, g)

  /**
   * Creates a characteristic function for the difference S_f \ S_g, where each set is given by
   * its characteristic function (f and g, respectively).
   * @param f the characteristic function of S_f
   * @param g the characteristic function of S_g
   * @return the characteristic function of S_f \ S_g
   */
  def difference(f: Int, g: Int): Int = and(f, not(g))

  /**
   * Checks whether the set with characteristic function f is contained in
   * or equal to the set with characteristic function g.
   * In other words, it is checked whether S_f  ⊆ S_g.
   * @param f characteristic function of S_f
   * @param g characteristic function of S_g
   * @return true iff S_f  ⊆ S_g holds
   */
  def isContained(f: Int, g: Int): Boolean = and(f, not(g)) == 0

  /**
   * Combines two functions, given by their instruction ids, recursively. See eq. 55 (p. 94) of Knuth's TAOCP (see class documentation).
   * @param operation the name to be used for caching operation results, has to be unique
   * @param f the instruction id of the first function
   * @param g the instruction id of the second function
   * @param commutative if true, instruction ids will ordered (to improve likelihood of a cache hit)
   * @param obviousSolution this is the 'core' of the method, as it implements the end of the recursion and is specific to the operation
   * @return the instruction id of the function "f∘g"
   */
  private[this] def compose(operation: String, f: Int, g: Int, commutative: Boolean = false)(obviousSolution: (Int, Int) => Option[Int]): Int = {
    requireInstrIds(f, g)

    /**
     * Depending on the given instruction's variable index and the minimal variable index in the synthesis,
     * select which instruction ids to consider for lower/higher branches.
     * @param id the instruction id
     * @param varIdx the variable index for the given instruction
     * @param minVarIdx the minimal variable index to be used in the synthesized function
     */
    def selectInstructions(id: Int, varIdx: Int, minVarIdx: Int): (Int, Int) = {
      if (varIdx == minVarIdx)
        (lowInstr(id), highInstr(id))
      else
        (id, id)
    }

    //If operation is commutative, make this pair unique via ordering 
    val (id1, id2) = if (commutative && f > g) (g, f) else (f, g)

    //Check for obvious solution (terminates recursion for trivial cases)
    obviousSolution(id1, id2).getOrElse {

      //Check cache for solutions
      solutionCache.getOrElseUpdate(id1 + operation + id2,
        {
          //'Melding' both function, see eq. 7.1.4.-(52) 
          val (var1Idx, var2Idx) = (variables(id1), variables(id2))
          val minVarIdx = {
            if (var1Idx == 0)
              var2Idx
            else if (var2Idx == 0)
              var1Idx
            else
              math.min(var1Idx, var2Idx)
          }
          val (id1LowInstr, id1HighInstr) = selectInstructions(id1, var1Idx, minVarIdx)
          val (id2LowInstr, id2HighInstr) = selectInstructions(id2, var2Idx, minVarIdx)

          //Recursively construct new function
          val lowInstrResult = compose(operation, id1LowInstr, id2LowInstr, commutative)(obviousSolution)
          val highInstrResult = compose(operation, id1HighInstr, id2HighInstr, commutative)(obviousSolution)
          unique(minVarIdx, lowInstrResult, highInstrResult)
        })
    }
  }

  /** Checks whether the given instruction ids are valid. */
  private[this] def checkInstrIds(ids: Int*) = ids.forall(x => x >= 0 && x < instrIdCounter)

  /** Requires valid instruction ids and constructs corresponding error message. */
  private[this] def requireInstrIds(ids: Int*) = require(checkInstrIds(ids: _*), "One instruction id in " + ids + " is not valid.")

  /**
   * Creates a structural representation of a function/set, given as a list of lines to easier support recursive nesting.
   * @param id the instruction id of the function
   * @param varNames the map from variable numbers to variable names
   * @param indent the string to be used for indentation
   * @return list of lines that represent the function in pseudo-code
   */
  def structureOf(id: Int, varNames: Map[Int, String] = Map(), indent: String = "\t"): List[String] = id match {
    case 0 => List()
    case 1 => List("true")
    case _ => {
      val thisVar = varNames.getOrElse(variables(id), id.toString)
      val lowBranch = structureOf(lowInstr(id), varNames, indent)
      val highBranch = structureOf(highInstr(id), varNames, indent)

      if (highBranch.isEmpty)
        "if(!" + thisVar + ") {" :: lowBranch.map(indent + _) ::: List("}")
      else {
        "if(" + thisVar + ") {" :: highBranch.map(indent + _) ::: (
          if (lowBranch.isEmpty) List("}") else List("}", "else {") ::: lowBranch.map(indent + _) ::: List("}"))
      }
    }
  }

  //TODO: Add methods for re-ordering?
}