/*
 * Copyright 2013 Roland Ewald
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
package alesia.utils.evaluation.expdata

import org.junit.Test
import org.junit.Assert._

import sessl._
import sessl.james._

class DefaultTauLeapingDataProvider(file: String) extends CachedRuntimeDataProvider(file,
    TauLeaping() scan (
      "epsilon" <~ range(0.01, 0.002, 0.05),
      "gamma" <~ range(5, 1, 15),
      "criticalReactionThreshold" <~ range(0, 5, 45)))

/**
 * Simple test for runtime data provider.
 *
 * @author Roland Ewald
 */
@Test class TestRuntimeDataProvider {

  @Test def testSimpleDataProvider() {
    
    val dp = new DefaultTauLeapingDataProvider("sample_runtime_data/sample_data_sr_autoreg_nw.csv")
    assertEquals(4, dp.algorithmParameters.size)
    assertEquals(20, dp.algorithmParameters(0).size)
    assertEquals(11, dp.algorithmParameters(1).size)
    assertEquals(1, dp.algorithmParameters(2).size)
    assertEquals(10, dp.algorithmParameters(3).size)
    assertEquals(9, dp.getRaw(0, 0, 0, 6).size)
    assertEquals(3.646666667, dp.get(0, 0, 0, 0), 10e-06)
  }

}