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

import org.junit.Test
import org.junit.Assert._

import BinaryDecisionNode._

/** Tests for basic entities of binary decision diagrams.
 *  @author Roland Ewald
 */
@Test
class TestBDDNode {

  /** The identity function, f(x) = x. */
  val id = BDDNode(0, FalseNode, TrueNode)

  /** The median function for three boolean variables. From Knuth's TAOCP, vol.4-1, p. 71.*/
  val median3 = {
    val node3 = BDDNode(2, FalseNode, TrueNode)
    BDDNode(0, BDDNode(1, FalseNode, node3), BDDNode(1, node3, TrueNode))
  }

  @Test
  def simpleBDDConstructionAndEvaluation() {
    //The constants:
    assertFalse(evaluate(FalseNode, Array[Boolean]()))
    assertTrue(evaluate(TrueNode, Array[Boolean]()))

    assertTrue(evaluate(id, Array(true)))
    assertFalse(evaluate(id, Array(false)))
  }

  @Test
  def conversionToInstructions() {
    val instructions = asInstructions(id)
    assertEquals(3, instructions.size)
    assertEquals(FalseNode.variable, instructions(0).variable)
    assertEquals(TrueNode.variable, instructions(1).variable)
    assertEquals(0, instructions(2).variable)

    assertEquals(6, asInstructions(median3).size)
  }

  @Test
  def instructionEvaluation() {
    assertTrue(evaluate(asInstructions(median3), Array(true, false, true)))
    assertFalse(evaluate(asInstructions(median3), Array(false, false, true)))
  }

}