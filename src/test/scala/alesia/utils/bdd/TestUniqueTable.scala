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

/**
 * Tests for the unique table implementation.
 * @author Roland Ewald
 */
@Test
class TestUniqueTable {

  /** Contains table to be tested. */
  trait TestTable {
    val table = new UniqueTable
  }

  @Test
  def simpleInserts() {
    new TestTable {
      //Inserting trivial nodes
      assertEquals(0, table.unique(1, 0, 0))
      assertEquals(1, table.unique(10, 1, 1))

      //Asserting first non-trivial node:
      assertEquals(2, table.unique(10, 0, 1))
    }
  }

  /** Contains some sample functions. */
  trait TestElements extends TestTable {
    //defining function v1 OR v2
    val instrIdV2or = table.unique(2, 1, 0)
    val instrIdV1or = table.unique(1, 1, instrIdV2or)

    //defining function v1 AND v2
    val instrIdV2and = table.unique(2, 1, 0)
    val instrIdV1and = table.unique(1, instrIdV2and, 0)

  }

  @Test
  def testElementsOK() {
    new TestElements {
      assertTrue(instrIdV2or == instrIdV2and)
      assertFalse(instrIdV1or == instrIdV1and)
    }
  }

  @Test
  def simpleEvaluation() {
    new TestElements {
      truthTableCheck(instrIdV1or, Array(false, true, true, true), table)
      truthTableCheck(instrIdV1and, Array(false, false, false, true), table)
    }
  }

  @Test
  def simpleAndSynthesis {
    new TestElements {
      assertEquals(instrIdV1and, table.and(instrIdV1or, instrIdV1and))
      assertEquals(0, table.and(instrIdV1or, 0))
      assertEquals(instrIdV1or, table.and(instrIdV1or, 1))
    }
  }

  @Test
  def simpleOrSynthesis {
    new TestElements {
      val v1 = table.unique(1, 1, 0)
      val v2 = table.unique(2, 1, 0)
      assertEquals(instrIdV1or, table.or(v1, v2))
    }
  }

  /** Checks a two-variable function against a simple truth table. */
  def truthTableCheck(id: Int, expected: Array[Boolean], table: UniqueTable) {
    assertEquals(expected(0), table.evaluate(id, Array(false, false)))
    assertEquals(expected(1), table.evaluate(id, Array(false, true)))
    assertEquals(expected(2), table.evaluate(id, Array(true, false)))
    assertEquals(expected(3), table.evaluate(id, Array(true, true)))
  }

}