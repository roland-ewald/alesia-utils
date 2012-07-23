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
package alesia.tools.results

import java.io.File
import scala.collection.mutable.ArraySeq
import sessl.james.tools.CSVFileWriter
import java.io.FileReader
import java.io.BufferedReader

import scala.collection.mutable.ListBuffer
import scala.collection.mutable.ArrayBuffer
import sessl.util.ResultOperations
import sessl.util.MiscUtils

/** Some simple methods for output aggregation.
 *  It is assumed that each file names contains the description of the data for a single setup.
 *  One setup can be picked to contain 'reference data', for comparing it with all other data sets.
 *  
 *  @param baseDirectory the basic working directory of the aggregator
 *  @param fileEnding the file ending of the data files that shall be aggregated
 *  @param targetDirectory the directory for aggregation output (relative to base directory)
 *
 *  @author Roland Ewald
 */
abstract class ResultAggregator[X](val baseDirectory: String, val fileEnding: String, val targetDirectory: String = "aggregated") {

  require(new File(baseDirectory).isDirectory(), "'" + new File(baseDirectory).getAbsolutePath() + "' is not a directory.")

  /** The files containing the data to be aggregated. */
  val dataFiles = new File(baseDirectory).list
    .filter(f => { val i = f.lastIndexOf('.'); i > 0 && i + 1 < f.length() && f.substring(i + 1).equalsIgnoreCase(fileEnding) })
    .sorted.map(f => new File(baseDirectory + File.separator + f)).filter(f => f.isFile && f.length > 0)

  /** The path to the target directory. */
  val targetDirectoryPath = setupTargetDirectory()

  /** User-defined aggregator functions. */
  type Aggregator = Function1[List[Seq[X]], List[_]]

  /** User defined aggregator functions that compare with the reference. */
  type RefAggregator = Function2[List[Seq[X]], List[Seq[X]], List[_]]

  /** The prefix for the output files. */
  var outputPrefix: String = "output_"

  /** The value delimiter to be used. */
  var delimiter = ','

  /** Flag whether to delete an existing target directory. */
  var deleteExistingTargetDir = false

  /** Reference data.*/
  private[this] var referenceData: Option[List[Seq[X]]] = None

  /** Map from tuple (aggregator, transpose_flag) to file writer. */
  private[this] val aggregators = scala.collection.mutable.Map[(Aggregator, Boolean), CSVFileWriter]()

  /** Map from tuple (references aggregator, transpose_flag) to file writers. */
  private[this] val refAggregators = scala.collection.mutable.Map[(RefAggregator, Boolean), CSVFileWriter]()

  /** Set of (the custom parts of) output file names to be used. */
  private[this] val outputFileNames = scala.collection.mutable.Set[String]()

  /** Determines whether reference data shall be included in the results. */
  def includeRefData = true

  /** Extracts the raw data. */
  def extractNumericData(line: String, del: Char = delimiter): ArraySeq[X] = line.split(del).map(parse)

  /** Extracts the name of the data source. */
  def extractName(file: File): String = file.getName()

  /** Parses a string to the given type. */
  def parse(number: String): X

  /** If comparison to reference results is necessary, this method returns the index of the file that contains it.
   *  @return the file containing the reference data
   */
  def pickReferenceDataFile(files: Seq[File]): Option[File] = None

  /** Carries out the aggregation. */
  def aggregate() {

    //Check for reference data
    val refDataFile = pickReferenceDataFile(dataFiles)
    val refAndTransposedData = readReferenceData(refDataFile)

    //Read data, write back aggregations
    for (file <- dataFiles if includeRefData || !refDataFile.isDefined || !refDataFile.get.equals(file)) {

      println("Analyzing file: " + file)

      val data = readDataFile(file)
      val transposedData = transpose(data)

      //Call aggregators
      aggregators.par.foreach(e => {
        e._2.store(extractName(file), if (e._1._2) e._1._1(transposedData) else e._1._1(data))
      })

      //Call reference-data comparing aggregators
      if (refDataFile.isDefined) {
        refAggregators.par.foreach(e => {
          e._2.store(extractName(file),
            if (e._1._2) e._1._1(refAndTransposedData.get._2, transposedData) else e._1._1(refAndTransposedData.get._1, data))
        })
      }
    }
  }

  /** Reads reference data. */
  def readReferenceData(file: Option[java.io.File]) = {
    if (file.isDefined) {
      val refData = readDataFile(file.get)
      Some((refData, transpose(refData)))
    } else None
  }

  /** Reads data file.
   *  @param dataFilee the data file
   *  @return lists of value sequences
   */
  def readDataFile(dataFile: File): List[Seq[X]] = {
    val br = new BufferedReader(new FileReader(dataFile))
    var dataToBeAggregated = List[Seq[X]]()
    try {
      var line = br.readLine()
      while (line != null) {
        val dataFromLine = extractNumericData(line)
        if (!filterData(dataFromLine, dataFile))
          dataToBeAggregated ::= dataFromLine
        line = br.readLine()
      }
    } finally {
      br.close()
    }
    dataToBeAggregated
  }

  /** Sequence to be filtered out. */
  def filterData(data: Seq[X], file: File) = false

  /** Registers aggregator function. */
  def addAggregator(name: String)(a: Aggregator) = {
    checkNameAvailability(name)
    aggregators((a, false)) = createFileWriter(name)
  }

  /** Registers aggregator that relies on transposed data. */
  def addTransposedAggregator(name: String)(a: Aggregator) = {
    checkNameAvailability(name)
    aggregators((a, true)) = createFileWriter(name)
  }

  /** Registers reference aggregator function. */
  def addRefAggregator(name: String)(a: RefAggregator) = {
    checkNameAvailability(name)
    refAggregators((a, false)) = createFileWriter(name)
  }

  /** Registers reference aggregator function. */
  def addRefTransposedAggregator(name: String)(a: RefAggregator) = {
    checkNameAvailability(name)
    refAggregators((a, true)) = createFileWriter(name)
  }

  /** Creates file writer. */
  def createFileWriter(name: String) = new CSVFileWriter(getOutputFilePath(name), false)

  /** Constructs the file name of an output file. */
  def getOutputFilePath(customName: String) = targetDirectoryPath + File.separator + outputPrefix + customName + '.' + fileEnding

  /** Checks availability of a custom name (they need to be unique). */
  def checkNameAvailability(name: String) = require(!outputFileNames(name), "Name '" + name + "' is not available anymore.")

  /** Transposes the data. */
  def transpose(data: List[Seq[X]]): List[Seq[X]] = {
    if (data.length == 0)
      return List[Seq[X]]()

    val rv = ArrayBuffer[ListBuffer[X]]()
    for (d <- data(0)) {
      rv += ListBuffer(d)
    }
    for (i <- 1 until data.length) {
      if (data(i).length != data(0).length)
        System.err.println("List with index " + i + " has " + data(i).length +
          " elements, but first list has " + data(0).length + " elements (need to be equal). \n\tList: " + data(i).mkString(","))
      val newColIt = data(i).iterator
      val rvIt = rv.iterator
      while (newColIt.hasNext && rvIt.hasNext) {
        rvIt.next.append(newColIt.next)
      }
    }
    rv.toList
  }

  /** Sets up the target directory. */
  private[this] def setupTargetDirectory() = {
    val targetDir = new File(baseDirectory + File.separator + targetDirectory)
    val targetDirPath = targetDir.getAbsolutePath()
    if (targetDir.exists && deleteExistingTargetDir)
      require(MiscUtils.deleteRecursively(targetDir), "Could not delete target directory (" + targetDirPath + ").")
    require(targetDir.exists || targetDir.mkdir(), "Could not create directory for output (" + targetDirPath + ").")
    targetDirPath
  }

  //Auxiliary methods for results handling:

  def mean(d: Seq[_]) = ResultAggregation(d).mean("")

  def stddev(d: Seq[_]) = ResultAggregation(d).stddev("")

  def absmax(d: Seq[_]) = ResultAggregation(d).absmax("")

  def absmin(d: Seq[_]) = ResultAggregation(d).absmin("")

  def max(d: Seq[_]) = ResultAggregation(d).max("")

  def min(d: Seq[_]) = ResultAggregation(d).min("")
}

/** Simple wrapper for ResultOperations. Is initialized with the data that shall be aggregated.
 *
 *  @see sessl.util.ResultOperations
 *
 *  @author Roland Ewald
 */
case class ResultAggregation(dataToBeAggregated: Seq[_]) extends ResultOperations {

  /** The data to be aggregated. */
  lazy val data = dataToBeAggregated.toList

  protected override def getValuesFor(varName: String) = data
}