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

import org.junit.Test
import org.junit.Assert._


/** Tests for the unique table implementation. 
 * @author Roland Ewald
 */
@Test
class TestUniqueTable {
  
  @Test
  def simpleInserts() {
    val table = new UniqueTable
    
    //Inserting trivial nodes
    assertEquals(0, table.unique(1, 0, 0))
    assertEquals(1, table.unique(10, 1, 1))
    
    //Asserting first non-trivial node:
    assertEquals(2, table.unique(10, 0, 1))
  }
  
}