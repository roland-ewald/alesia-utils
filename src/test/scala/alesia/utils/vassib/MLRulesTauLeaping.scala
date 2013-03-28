package alesia.utils.vassib

import org.junit.Test
import org.junit.Assert._
import sessl.util.ScalaToJava._

import alesia.utils.results.ResultAggregator
import java.io.File
import alesia.utils.math.JensenShannonDivergence
import org.jamesii.core.math.statistics.tests.wilcoxon.WilcoxonRankSumTest
import scala.collection.mutable.ListBuffer

/** Test application to real-world data. */
class VassibAggregator extends ResultAggregator[Int]("./cellaxinp", "csv") {
  override def parse(s: String): Int = java.lang.Integer.parseInt(s.trim)
  override def pickReferenceDataFile(files: Seq[File]): Option[File] = {
    for (f <- files)
      if (f.getName().contains("MLRulesRevised"))
        return Some(f);
    None
  }
  override def extractName(file: File) = {
    var fName = file.getName()
    if (fName.indexOf('_') > 0 && fName.indexOf('_') + 1 < fName.length())
      fName = fName.substring(fName.lastIndexOf('_') + 1);
    fName.substring(0, fName.lastIndexOf(".csv"))
  }
}

object MLRulesTauLeaping extends App {

  val aggregator = new VassibAggregator() {
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

    //    /** Controls trajectory length. */
    //    override def filterData(data: Seq[Int], file: File) = { val rv = data.length != 501; if (rv) println("Filtered line from " + file.getName() + " - length is " + data.length); rv }
  }

  aggregator.aggregate()

}