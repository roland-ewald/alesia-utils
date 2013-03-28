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

import org.junit.Test
import org.junit.Assert._
import sessl.util.ScalaToJava._
import scala.collection.mutable.ListBuffer
import org.jamesii.core.math.statistics.tests.wilcoxon.WilcoxonRankSumTest
import java.io.File

import alesia.utils.results.ResultAggregator
import alesia.utils.math.JensenShannonDivergence

/** Tests for the ResultAggregator.
 *  @author Roland Ewald
 */
@Test class TestResultAggregator {

  /** Working directory containing the test input to be aggregated. */
  val workingDirectory = "./sample_input"

  /** File ending used in the tests. */
  val testFileEnding = "csv"

  /** Test implementation. */
  class TestResultAggregator extends ResultAggregator[Int](workingDirectory, testFileEnding) {
    override def parse(s: String): Int = java.lang.Integer.parseInt(s.trim)
    override def pickReferenceDataFile(files: Seq[File]) = Some(files.head) //Just for testing
    override def extractName(file: File) = {
      var fName = file.getName()
      if (fName.indexOf('_') > 0 && fName.indexOf('_') + 1 < fName.length())
        fName = fName.substring(fName.lastIndexOf('_') + 1);
      fName.substring(0, fName.lastIndexOf('.' + testFileEnding))
    }
  }

  /** Tests transposition. */
  @Test def transposition() = {
    val aggregator = new TestResultAggregator()
    assertEquals(0, aggregator.transpose(List()).size)
    assertEquals(List(Seq(1, 4), Seq(2, 5), Seq(3, 6)), aggregator.transpose(List(Seq(1, 2, 3), Seq(4, 5, 6))))
  }

  /** Test some simple aggregation methods. */
  @Test def executeSimpleAggregation() = {
    val aggregator = new TestResultAggregator() {

      addTransposedAggregator("mean")(x => x.map(mean))
      addTransposedAggregator("stddev")(_.map(stddev))
      addTransposedAggregator("absmin")(_.map(absmin))
      addTransposedAggregator("absmax")(_.map(absmax))

      //Wilcoxon-Ranksum p-value for every time point
      addRefTransposedAggregator("wilcoxon_ranksum_pvalues") {
        (referenceData, data) =>
          {
            for (compareData <- referenceData zip data) yield {
              new WilcoxonRankSumTest().executeTest(toIntegerList(compareData._1), toIntegerList(compareData._2))
            }
          }
      }

      //Jensen-Shannon divergence for every time point
      addRefTransposedAggregator("jensen_shannon_div") {
        (referenceData, data) => (referenceData zip data).map(t => JensenShannonDivergence(t._1, t._2))
      }

      //Some single metrics to characterize accuracy
      addRefTransposedAggregator("single_uncertainty_metrics") {
        (referenceData, data) =>
          {
            val rv = ListBuffer[Number]()

            val refMeans = referenceData.map(mean)
            val refMeansMean = mean(refMeans)
            val refStdDevs = referenceData.map(stddev)
            val refStdDevsMean = mean(refStdDevs)

            val dataMeans = data.map(mean)
            val dataMeansMean = mean(dataMeans)
            val dataStdDevs = data.map(stddev)
            val dataStdDevsMean = mean(dataStdDevs)

            assertEquals("Reference trajectories and comparison trajectories should have the same number of data points.",
              refMeans.length, dataMeans.length)

            //RMSE means / std devs
            val squareError = { pair: (Double, Double) => (pair._1 - pair._2) * (pair._1 - pair._2) }
            val meansRMSE = scala.math.sqrt(mean(refMeans.zip(dataMeans).map(squareError)))
            val stdDevRMSE = scala.math.sqrt(mean(refStdDevs.zip(dataStdDevs).map(squareError)))

            //CV(RMSE)
            val cvMeansRMSE = meansRMSE / mean(List(refMeansMean, dataMeansMean))
            val cvStdDevRMSE = stdDevRMSE / mean(List(refStdDevsMean, dataStdDevsMean))

            //Normalized RMSE
            val allMeans = refMeans ::: dataMeans
            val maxMean = max(allMeans)
            val minMean = min(allMeans)
            val nMeansRMSE = meansRMSE / (maxMean - minMean)

            val allStdDevs = refStdDevs ::: dataStdDevs
            val maxStdDev = max(allStdDevs)
            val minStdDev = min(allStdDevs)
            val nStdDevRMSE = stdDevRMSE / (maxStdDev - minStdDev)

            //Jensen-Shannon divergence in last observed state
            val wrPValEnd = new WilcoxonRankSumTest().executeTest(toIntegerList(referenceData.last), toIntegerList(data.last))
            val jsDivEnd = JensenShannonDivergence(referenceData.last, data.last)

            //Absolute differences on simple aggregate means
            rv += scala.math.abs(refMeansMean - dataMeansMean)
            rv += scala.math.abs(refStdDevsMean - dataStdDevsMean)
            rv += meansRMSE
            rv += stdDevRMSE
            rv += cvMeansRMSE
            rv += cvStdDevRMSE
            rv += nMeansRMSE
            rv += nStdDevRMSE
            rv += wrPValEnd
            rv += jsDivEnd
            rv.toList
          }
      }

      /** Controls trajectory length. */
      override def filterData(data: Seq[Int], file: File) = { val rv = data.length != 501; if (rv) println("Filtered line from " + file.getName() + " - length is "+data.length); rv }
    }
    
    assertEquals(4, aggregator.dataFiles.length)
    aggregator.aggregate()
  }

}