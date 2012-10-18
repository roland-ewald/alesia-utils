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
import sessl.util.Logging
import UniqueTable._

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

    //Defining variables v1 and v2
    val v1 = table.unique(1, 0, 1)
    val v2 = table.unique(2, 0, 1)

    //Defining function v1 OR v2
    val instrIdV2or = table.unique(2, 0, 1)
    val instrIdV1or = table.unique(1, instrIdV2or, 1)

    //Defining function v1 AND v2
    val instrIdV2and = table.unique(2, 0, 1)
    val instrIdV1and = table.unique(1, 0, instrIdV2and)

    assert(bddIsValid(instrIdV1and, table))

  }

  @Test
  def testElementsOK() {
    new TestElements {
      assertTrue(v2 == instrIdV2or && instrIdV2or == instrIdV2and)
      assertTrue(instrIdV1or != instrIdV1and)
    }
  }

  @Test
  def simpleEvaluation() {
    new TestElements {
      assert(truthTableCheck(instrIdV1or, Array(false, true, true, true), table))
      assert(truthTableCheck(instrIdV1and, Array(false, false, false, true), table))
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
      assertEquals(instrIdV1or, table.or(v1, v2))
    }
  }

  @Test
  def simpleXorAndNotSynthesis {
    new TestElements {
      assert(truthTableCheck(table.xor(v1, v2), Array(false, true, true, false), table))
      assert(truthTableCheck(table.not(table.xor(v1, v2)), Array(true, false, false, true), table))
    }
  }

  @Test
  def simpleImpliesSynthesis {
    new TestElements {
      assert(truthTableCheck(table.implies(v1, v2), Array(true, true, false, true), table))
    }
  }

  @Test
  def containsWorks {
    new TestElements {
      assertTrue(table.isContained(table.and(v1, v2), table.or(v1, v2)))
      assertFalse(table.isContained(table.or(v1, v2), table.and(v1, v2)))
    }
  }

  @Test
  def setOperationsWorks {
    new TestElements {
      assertEquals(table.and(v1, v2), table.intersection(table.or(v1, v2), table.and(v1, v2)))
      assertEquals(table.or(v1, v2), table.union(table.or(v1, v2), table.and(v1, v2)))
      assertEquals(table.xor(v1, v2), table.difference(table.or(v1, v2), table.and(v1, v2)))
      assertTrue(table.isEmpty(table.and(v1, table.not(v1))))
    }
  }

  @Test
  def simpleSubstitutionWorks {
    new TestElements {
      val v3 = table.unique(3, 0, 1)
      val v4 = table.unique(4, 0, 1)
      val instrIdV3orV4 = table.unique(3, v4, 1)
      assertEquals(instrIdV1or, table.substitute(instrIdV3orV4, Map(3 -> 1, 4 -> 2)))
    }
  }

  @Test
  def existsQuantifierWorks {
    new TestElements {
      assert(truthTableCheck(table.exists(List(1), table.or(v1, v2)), Array(true, true, true, true), table))
      assert(truthTableCheck(table.exists(List(2), table.and(v1, v2)), Array(false, false, true, true), table))
    }
  }

  @Test
  def forAllQuantifierWorks {
    new TestElements {
      assert(truthTableCheck(table.forall(List(1), table.or(v1, v2)), Array(false, true, false, true), table))
      assert(truthTableCheck(table.forall(List(2), table.and(v1, v2)), Array(false, false, false, false), table))
    }
  }

  @Test
  def variablesOf {
    new TestElements {
      assert(table.variablesOf(instrIdV1and) == List(1, 2))
      assert(table.variablesOf(instrIdV2or) == List(2))
      assert(table.variablesOf(instrIdV1and, instrIdV1or) == List(1, 2))
    }
  }

}