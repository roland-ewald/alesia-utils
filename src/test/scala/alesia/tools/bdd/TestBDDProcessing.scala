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

/** Tests the algorithms to manage and process binary decision diagrams.
 *
 *  @author Roland Ewald
 */
@Test
class TestBDDProcessing {

  import BDDProcessing._
  import TestBinaryDecisionNode._

  @Test
  def solutionCounting() {
    assertEquals(1, countSolutions(id))
    assertEquals(4, countSolutions(median3))
    assertEquals(4, countSolutions(median3unreduced))
  }

  @Test
  def reductionMedian3() {
    val reducedMedian3 = reduce(median3unreduced)
    assertEquals(7, reducedMedian3.size)
    checkMedian3(reducedMedian3)

    val secondReduction = reduce(reducedMedian3)
    assertEquals(reducedMedian3.size, secondReduction.size)
    checkMedian3(secondReduction)
  }

  @Test
  def reductionId() {
    val reducedId = reduce(id)
    checkInstructions(reducedId)
    assertEquals(BinaryDecisionNode.asInstructions(id).size, reducedId.size)
  }

}