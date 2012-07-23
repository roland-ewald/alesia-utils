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
package alesia.tools.math

import org.junit.Test
import org.junit.Assert._

import Constants._

/** Test for Shannon entropy.
 *  @author Roland Ewald
 */
@Test class TestShannonEntropy {

  @Test def testShannonEntropy() {
    assertEquals(1.60943791, ShannonEntropy(Seq(5, 4, 3, 2, 1)), epsilon)
    assertEquals(2.24595208, ShannonEntropy(Seq(1, 4, 22, 90, 2, 2, 4, 5, 6, 7, 7, 9, 8, 8, 9)), epsilon)
  }
}