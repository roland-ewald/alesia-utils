

package alesia.componentrating.james

import sessl.util.Logging
import junit.framework.Assert._
import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner
import org.scalatest.FunSpec
import alesia.componentrating.misc.Serializer

/**
 * Creates a file which stores the real ranking of simulator components.
 * Active ranking later loads these to calculate the distance ("progress").
 * 
 * Before running the Activ Ranking the file created by this test/class must be copied to the working directory.
 * 
 * @author Jonathan Wienss
 *
 */
@RunWith(classOf[JUnitRunner])
class TestCreateRealRankingFile extends FunSpec with Logging {

  val realRanking = List(
    ("eventqueues.mlist.MListFactory", 1),
    ("org.jamesii.core.util.eventset.LinkedListEventQueueFactory", 2),
    ("eventqueues.twolist.TwoListFactory", 3),
    ("eventqueues.calendarqueue.CalendarQueueFactory", 4),
    ("org.jamesii.core.util.eventset.BucketsThresholdEventQueueFactory", 5),
    ("eventqueues.medianpointer.MedianPointerLinkedListEventQueueFactory", 6),
    ("eventqueues.calendarqueue.CalendarReQueueFactory", 7),
    ("org.jamesii.core.util.eventset.HeapEventQueueFactory", 8),
    ("org.jamesii.core.util.eventset.SimpleEventQueueFactory", 9),
    ("eventqueues.dynamiccalendarqueue.DynamicCalendarQueueFactory", 10))

  describe("File Ranking") {
    it("can be stored and loaded") {
      Serializer.toFile("realRankingFile.xml", realRanking)
      val news: List[(java.lang.String, Int)] = Serializer.fromFile("realRankingFile.xml")
      assertEquals(realRanking, news)
      logger.info("Ranking loaded:" + news.mkString(","))
    }
  }
}