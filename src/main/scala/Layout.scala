import java.util.Date
import math._
import scala.Some
import DateConversion._

object Layout {
  val millisecondsPerDay = 1000d * 60 * 60 * 24
//  private val targetRatio = 1 / 1.61803398875
  val targetRatio = 8.5 / 11

  private object fitnessWeights {
    val targetRatio = 0.3d
    val dateAlignment = 1d * (1 / millisecondsPerDay)
  }


  val dateAlignmentBias = 0

  def apply(cats: List[Category]): List[Block] = {

    val sortedCats = cats.sortBy(_.items.size)

    val spacerAtTopCategory = 1.5
    val topSize = sortedCats.last.items.size
    val topAmt = sortedCats.last.items.map(_.amt).sum

    val items = cats.map(_.items).flatten
    val dates = items.map(_.date)
    val days = (dates.max - dates.min) / millisecondsPerDay
    val width = topSize * spacerAtTopCategory
    val widthPerDay = width / days

    val amtToArea = width / topAmt

    def score(b: Block, nextDates: List[Date]): Double = {
      // smaller is better, zero is best
      val x = fitnessWeights.targetRatio * abs(b.idealWidth(targetRatio) - b.width)
      val y = (for(d <- nextDates) yield {
                val blockRightEdgeDate = b.item.date + b.width / widthPerDay
                fitnessWeights.dateAlignment * max(0, blockRightEdgeDate - d)
              }).sum
      val result = x + y
      println("  score %.2f + %.2f = %f  :  %s".format(x, y, result, b))
      result
    }

    def optimizeOnce(b: Block, above: List[Block], nextDateHere: Option[Date]): (Block, List[Block]) = {

      def option(candidate: Block, bias: Double, nextDates: List[Date], r: List[Block]) =
        Some(score(candidate, nextDates) - bias, candidate, r)

      val aboveDate = above.headOption.map(_.firstDate).toList
      println("optimizeOnce: %s %s".format(aboveDate, nextDateHere))
      val bothDates = aboveDate ++ nextDateHere.toList

      val optionAdd = if (above.isEmpty) None
                      else option(b.add(above.head), dateAlignmentBias, nextDateHere.toList, above.tail)

      val optionWiden = option(b.widen, 0, bothDates, above)

      val optionNoop = option(b, 0, bothDates, above)

      val choice = List(optionAdd, optionWiden, optionNoop).maxBy(_.map(-_._1)).get
      (choice._2, choice._3)
    }

    def optimize(b: Block, above: List[Block], nextDate: Option[Date]): (Block, List[Block]) = {
      val (newB, newOther) = optimizeOnce(b, above, nextDate)
      if (b eq newB) (b, newOther)
      else optimize(newB, newOther, nextDate)
    }

    def zipRows(here: List[Block], above: List[Block]): List[Block] = {
      require(here.size <= above.size)

      def loop(here: List[Block], above: List[Block]): List[Block] = here match {
        case Nil => List()
        case here1 :: Nil =>
          List(above.foldLeft(here1) { _.add(_) })
        case here1 :: here2 :: tail =>
          val (b, restAbove) = optimize(here1, above, Some(here2.item.date))
          b :: loop(here2 :: tail, restAbove)
      }

      loop(here, above)
    }

    def makeBlocks(c: Category) = for(i <- c.items) yield Block(i.amt * amtToArea, i)
    def loop(cats: List[Category]): List[Block] = cats match {
      case Nil => List()
      case hd :: Nil => makeBlocks(hd).map(optimize(_, List(), None)._1)
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
