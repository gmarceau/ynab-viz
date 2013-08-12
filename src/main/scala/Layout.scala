import java.util.Date
import math._
import scala.Some
import DateConversion._

object Layout {
  val millisecondsPerDay = 1000d * 60 * 60 * 24
//  private val targetRatio = 1 / 1.61803398875
  val targetRatio = 8.5 / 11  // height over width

  private object fitnessWeights {
    val targetRatio = 0.5d
    val dateAlignment = 10d
  }


  val dateAlignmentBias = 0

  def apply(cats: List[Category]): List[Block] = {

    val sortedCats = cats.sortBy(_.items.size) // .drop(cats.size-5)

    val spacerAtTopCategory = 1.5
    val topSize = sortedCats.last.items.size
    val topAmt = sortedCats.last.items.map(_.amt).sum

    val items = cats.map(_.items).flatten
    val dates = items.map(_.date)
    val firstDate = dates.min
    val days = (dates.max - firstDate) / millisecondsPerDay
    val width = topSize * spacerAtTopCategory
    val widthPerDay = width / days

    val amtToArea = width / topAmt

    def widthToDays(w: Int) = w / widthPerDay * millisecondsPerDay

    def score(b: Block, hereZ: Double, aboveZ: Double, consumes: Boolean): Double = {
      // smaller is better, zero is best
      val v1 = fitnessWeights.targetRatio * abs(b.idealWidth(targetRatio) - b.width)

      val v2 = fitnessWeights.dateAlignment * (if (consumes) hereZ - aboveZ else aboveZ - hereZ)

      val result = v1 + v2
      println("  score %.2f + %.2f = %f  :  %s".format(v1, v2, result, b))
      result
    }

    def minDate(aboveDate: Option[Date], nextDate: Option[Date]): Option[Date] = (aboveDate, nextDate) match {
      case (None, None) => None
      case (d, None) => d
      case (None, d) => d
      case (Some(a), Some(b)) => Some(min(a, b))
    }


    def optimizeOnce(b: Block, above: List[Block], hereZ: Double, aboveZ: Double): (Block, List[Block]) = {
      println("optimizeOnce " + hereZ + " " + aboveZ)
      def option(candidate: Block, bias: Double, consumes: Boolean, r: List[Block]) =
        Some(score(candidate, hereZ, aboveZ, consumes) - bias, candidate, r)

      val optionAdd = if (!above.isEmpty)
                        option(b.add(above.head), 0.1, true, above.tail)
                      else None

      val optionStack = if (!above.isEmpty && above.head.top.isEmpty && above.head.width >= b.topWidth)
                          option(b.stack(above.head), 0.2, true, above.tail)
                        else None

      val optionWiden = option(b.widen, 0, false, above)
      val optionNoop = option(b, 0, false, above)

      val choice = List(optionAdd, optionStack, optionWiden, optionNoop).maxBy(_.map(-_._1)).get
      (choice._2, choice._3)
    }

    def zipRows(here: List[Block], above: List[Block]): List[Block] = {
      require(here.size <= above.size)
      val startHereZ = here.size
      val startAboveZ = above.size

      def optimize(b: Block, above: List[Block], hereZ: Double, aboveZ: Double): (Block, List[Block]) = {
        val (newB, newAbove) = optimizeOnce(b, above, hereZ, aboveZ)
        if (b eq newB) (b, newAbove)
        else optimize(newB, newAbove, hereZ, newAbove.size.toDouble / startAboveZ)
      }

      def loop(here: List[Block], above: List[Block], hereZ: Double, aboveZ: Double): List[Block] = here match {
        case Nil => List()
        case hd :: Nil =>
          List(above.foldLeft(hd) { _.add(_) })
        case hd :: tail =>
          println("loop " + hereZ + " " + aboveZ)
          val (b, restAbove) = optimize(hd, above, hereZ/startHereZ, aboveZ/startAboveZ)
          b :: loop(tail, restAbove, hereZ - 1, restAbove.size)
      }

      loop(here, above, here.size-1, above.size)
    }

    def optimizeStart(b: Block): (Block, List[Block]) = {
      val (newB, newAbove) = optimizeOnce(b, List(), 0, 0)
      if (b eq newB) (b, newAbove)
      else optimizeStart(newB)
    }


    def makeBlocks(c: Category) = for(i <- c.items) yield Block(i.amt * amtToArea, i)
    def loop(cats: List[Category]): List[Block] = cats match {
      case Nil => List()
      case hd :: Nil => makeBlocks(hd).map(optimizeStart).map(_._1)
      case hd :: tl =>
        val sub = loop(tl)
        println("== zipRow " + hd.name + "==")
        val result = zipRows(makeBlocks(hd), sub)
        println("== zipRow result " + hd.name + "==")
        result.foreach(println)
        result
    }
    loop(sortedCats)
  }


}
