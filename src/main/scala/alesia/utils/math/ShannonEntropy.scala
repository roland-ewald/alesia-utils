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
package alesia.utils.math

/** Calculation of Shannon Entropy (logarithm base is e). See http://en.wikipedia.org/wiki/Shannon_entropy.
 *  @author Roland Ewald
 */
object ShannonEntropy {

  /** Calculate the Shannon entropy.*/
  def entropy(data: Seq[Int]): Double = {
    require(data.size > 0, "List of data must not be empty.")
    val counter = scala.collection.mutable.Map[Int, Int]()
    for (d <- data) counter(d) = counter.getOrElse(d, 0) + 1
    val numData = data.length.toDouble
    entropyFormula(counter.map(_._2 / numData))
  }

  /** The entropy formula as such. Call this in case the probability mass function is already given,
   *  i.e. the p_i have already been calculated.
   */
  def entropyFormula(pis: Iterable[Double]) = -pis.foldLeft(0.0)((x, y) => x + y * scala.math.log(y))

  /** Simplified access. */
  def apply(data: Seq[Int]) = entropy(data)

}