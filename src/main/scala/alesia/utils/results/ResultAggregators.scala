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
package alesia.utils.results

import org.jamesii.core.math.statistics.tests.wilcoxon.WilcoxonRankSumTest
import sessl.util.ScalaToJava._
import alesia.utils.math.JensenShannonDivergence

/**
 * Some aggregators that are often useful. To be used within a ResultAggregator.
 *
 * @see ResultAggregator
 *
 * @author Roland Ewald
 */
object ResultAggregators {

  type Data = List[Seq[Int]]

  val wilcoxon = { (referenceData: Data, data: Data) =>
    {
      for (compareData <- referenceData zip data) yield {
        new WilcoxonRankSumTest().executeTest(toIntegerList(compareData._1), toIntegerList(compareData._2))
      }
    }
  }

  val jsDivergence = { (referenceData: Data, data: Data) => (referenceData zip data).map(t => JensenShannonDivergence(t._1, t._2)) }

}