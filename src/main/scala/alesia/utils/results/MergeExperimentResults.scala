package alesia.utils.results

import java.io.File
import alesia.componentrating.misc.Serializer
import sessl.util.Logging
import sessl.james.tools.CSVFileWriter
import scala.collection.mutable.ListBuffer
import alesia.componentrating.activeRanking.Round
import alesia.componentrating.activeRanking.misc.HammingDistance
import scala.util.control.Breaks._
import alesia.componentrating.activeRanking.misc.HammingDistance

/**
 * @author Roland Ewald
 *
 * import alesia.componentrating.activeRanking.Round
 */
object MergeExperimentResults extends App with Logging {

  val resultFolder = "./results"

  val summaryFile = new CSVFileWriter("./result_summary.csv", append = false)

  for (file <- new File(resultFolder).listFiles() if file.getName().endsWith(".xml") && !file.getName.contains("ResultSummaryFile.xml")) {
    logger.info("Analyzing '" + file + "'")
    var round: Round = null

    breakable {
      try { round = Serializer.fromFile(file.getAbsolutePath()) } catch { case e: Exception => break }

      logger.info("Replications found:" + round.info.size)

      //Read results
      val hammingDistances = ListBuffer[List[Int]]()
      val inversionDistances = ListBuffer[List[Int]]()
      for (replication <- round.info.keySet.toList.sorted) {
        val roundData = round.info(replication)
        val data = for (singleRound <- roundData.keySet.toList.sorted) yield {
          val singleRoundData = roundData(singleRound).toList
          require(singleRoundData.length == 2)
          val firstElem = singleRoundData.head
          val secondElem = singleRoundData.tail.head
          if (firstElem._1.getClass().getCanonicalName().equals(classOf[HammingDistance].getCanonicalName())) {
            (firstElem._2, secondElem._2)
          } else {
            (secondElem._2, firstElem._2)
          }
        }
        hammingDistances.append(data.map(_._1))
        inversionDistances.append(data.map(_._2))
      }

      //Merge results
      val allHDists = mergeListBuffers(hammingDistances)
      val allInvDists = mergeListBuffers(inversionDistances)

      //Store average distances:
      summaryFile << List(beautyfyFileName(file.getName), "Hamming") ::: allHDists.map(averageResults)
      summaryFile << List(beautyfyFileName(file.getName), "Inversions") ::: allInvDists.map(averageResults)
    }
  }

  def beautyfyFileName(fileName: String) = "\"" + fileName.replaceAll(",", ";") + "\""

  def averageResults(vals: ListBuffer[Int]) = {
    val list = vals.toList
    list.sum / list.length.toDouble
  }

  def mergeListBuffers(data: ListBuffer[List[Int]]) = data.tail.foldLeft(data.head.map(ListBuffer(_)))(merge)

  def merge(lists: List[ListBuffer[Int]], list: List[Int]): List[ListBuffer[Int]] = {
    (lists zip list).map(x => { x._1.append(x._2); x._1 })
  }
}