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

import sessl.Simulator
import scala.io.Source
import sessl.util.CreatableFromVariables

class CachedRuntimeDataProvider(fileName: String, algos: Iterable[_ <: Simulator with CreatableFromVariables[_]],
  aggregate: List[Double] => Double = (xs => xs.sum / xs.size)) extends RuntimeDataProvider {

  type RuntimeData = Map[AlgoID, List[Double]]

  val algorithms: Map[AlgoID, Simulator] = algos.map(a => (a.toString, a)).toMap

  val paramsByAlgo = algorithms.keys.map(s => (s, s.substring(s.indexOf('(') + 1, s.indexOf(')')).split(',').map(_.toDouble)))

  val algoByParams: Map[List[Double], AlgoID] = paramsByAlgo.map(x => (x._2.toList, x._1)).toMap

  val algorithmParameters: Array[Array[Double]] = {
    val valuesPerParamIndex = scala.collection.mutable.Map[Int, Set[Double]]()
    for (pt <- paramsByAlgo; elem <- pt._2.zipWithIndex)
      valuesPerParamIndex(elem._2) = Set(elem._1) ++ valuesPerParamIndex.getOrElse(elem._2, Set())
    valuesPerParamIndex.map(x => (x._1, x._2.toArray.sorted)).toList.sortBy(_._1).map(_._2).toArray
  }

  val data: RuntimeData = {
    val file = Source.fromFile(fileName)
    val data = (for (l <- file.getLines) yield {
      val quotedLineElems = l.split("\"")
      val algoID: AlgoID = quotedLineElems(1)
      val data = quotedLineElems(2).split(",")
      (algoID, if (algorithms.contains(algoID)) data.tail.toList.map(_.toDouble) else List())
    }).filter(_._2.nonEmpty).toMap
    file.close()
    data
  }

  def parameterValueNumbers: Array[Int] = algorithmParameters.map(_.size)

  def algoIDForParamNums(paramNumbers: Int*) = {
    require(paramNumbers.size == algorithmParameters.size, "Each parameter must be given")
    val params = (for (pn <- paramNumbers.zipWithIndex) yield algorithmParameters(pn._2)(pn._1)).toList
    algoByParams(params)
  }

  override def algorithmIDs = algorithms.keySet

  override def get(a: AlgoID): Double = aggregate(data(a))

  def get(paramNumbers: Int*): Double = get(algoIDForParamNums(paramNumbers: _*))

  override def getRaw(a: AlgoID): List[Double] = data(a)

  def getRaw(paramNumbers: Int*): List[Double] = getRaw(algoIDForParamNums(paramNumbers: _*))
}