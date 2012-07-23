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
import scala.util.Random

import org.junit.Assert.assertTrue
import org.junit.Test

/** Test for Jensen-Shannon divergence.
 *  @author Roland Ewald
 */
@Test class TestJensenShannonDivergence {

  /** The size of the sample data for which the divergence is computed. */
  val sampleSize = 1000

  /** Number of test trials.*/
  val trialNumber = 1000

  @Test def testJSDivergence() {
    val rng = new Random()
    def getSample(bound: Int = 100) = for (i <- 0 until sampleSize) yield rng.nextInt(bound)
    def inRange(d: Double) = d >= 0. && d <= 1.
    for (i <- 0 until trialNumber) {
      val similarDiv = JensenShannonDivergence(getSample(), getSample())
      val unsimilarDiv = JensenShannonDivergence(getSample(), getSample(10))
      assertTrue("Should always be in [0,1]. ", inRange(similarDiv) && inRange(unsimilarDiv))
      assertTrue("The more un-similar the distributions, the larger the divergence. ", unsimilarDiv > similarDiv)
    }
  }
}