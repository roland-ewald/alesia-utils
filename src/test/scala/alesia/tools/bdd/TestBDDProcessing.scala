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

  @Test
  def solutionCounting() {
    assertEquals(1, countSolutions(TestBDDNode.id))
    assertEquals(4, countSolutions(TestBDDNode.median3))
  }
  
  @Test
  def reduction() {
    reduce(TestBDDNode.median3)
  }

}