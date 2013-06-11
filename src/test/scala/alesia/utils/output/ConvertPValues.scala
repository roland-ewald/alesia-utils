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
package alesia.utils.output
import org.jamesii.core.util.misc.CSVReader
import sessl.james.tools.CSVFileWriter
import java.io.BufferedReader
import java.io.FileReader

/** Conversion of aggregated p-values to discrete significance levels.
 *  @author Roland Ewald
 */
class ConvertPValues

object ConvertPValues extends App {

  //The input file
  val inputFile = "./vassib_input/aggregated/output_wilcoxon_ranksum_pvalues.csv"
    
  //The thresholds
  val significanceThresholds = List(.001, .01, .05, 1)

  val csvReader = new BufferedReader(new FileReader(inputFile))
  val csvWriter = new CSVFileWriter(inputFile + ".processed", true)

  try {
    var line = csvReader.readLine()
    while (line != null) {

      val endOfSetupDesc = line.lastIndexOf("\"")
      val setup = line.substring(0, endOfSetupDesc + 1)

      val lineContent = line.substring(endOfSetupDesc + 2).split(",").map {
        element =>
          val elem = java.lang.Double.parseDouble(element)
          significanceThresholds.zipWithIndex.filter(tuple => { elem <= tuple._1 }).map(_._2).head
      }

      csvWriter << List(setup) ::: lineContent.map(_.toString).toList
      line = csvReader.readLine()
    }
    csvReader.close
  } catch {
    case e:Throwable => e.printStackTrace()
  }

}