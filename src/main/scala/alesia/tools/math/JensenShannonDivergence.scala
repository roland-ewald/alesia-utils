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

import alesia.tools.math.ShannonEntropy.entropy
import alesia.tools.math.ShannonEntropy.entropyFormula

/** Calculation of Jensen-Shannon divergence (http://en.wikipedia.org/wiki/Jensen-Shannon_divergence).
 *  It can be used to compare probability distributions and, unlike Kullback-Leibler divergence,
 *  it is defined even when Q(i) = 0 for some P(i) > 0 (e.g. see http://mathoverflow.net/questions/72672).
 *
 *  @author Roland Ewald
 */
object JensenShannonDivergence {

  /** Calculates Jensen-Shannon divergence. P and Q are both weighted by 0.5. */
  def divergence(p: Seq[Int], q: Seq[Int]) = {
    require(p.length > 0 && q.length > 0, "Non-empty data sequences required.")
    entropy(p ++ q) - 0.5 * entropy(p) - 0.5 * entropy(q)
  }

  /** Simplified access. */
  def apply(p: Seq[Int], q: Seq[Int]) = divergence(p, q)

}