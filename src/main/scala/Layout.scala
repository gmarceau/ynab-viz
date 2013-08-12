import java.util.Date
import math._
import scala.Some
import DateConversion._

object Layout {
  val millisecondsPerDay = 1000d * 60 * 60 * 24
//  private val targetRatio = 1 / 1.61803398875
  val targetRatio = 8.5 / 11

  private object fitnessWeights {
    val targetRatio = 1d
    val dateAlignment = 1d * (1 / millisecondsPerDay)
  }


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
    println("amtToArea = " + amtToArea)

    def score(b: Block, nextDate: Option[Date]): Double = {
      // smaller is better, zero is best
      val x = fitnessWeights.targetRatio * abs(b.ratio - targetRatio)
      val y = nextDate match { case Some(d) =>
        val blockRightEdgeDate = b.weightedDate + b.width / widthPerDay
        fitnessWeights.dateAlignment * max(0, blockRightEdgeDate - d)
      case None => 0
      }
      val result = x + y
      println("  score %.2f + %.2f = %f  : %s".format(x, y, result, b))
      result
    }

    def smallerDate(fst: Option[Date], snd: Option[Date]): Option[Date] = (fst, snd) match {
      case (None, None) => None
      case (None, d) => d
      case (d, None) => d
      case (Some(a), Some(b)) => Some(List(a, b).min)
    }

    def optimizeOnce(b: Block, above: List[Block], nextDateHere: Option[Date]): (Block, List[Block]) = {
      println("optimizeOnce " + b)
      val nextDate = smallerDate(above.headOption.map(_.item.date), nextDateHere)
      def option(candidate: Block) = Some(score(candidate, nextDate) -> candidate)
      val optionNoop = option(b)
      val optionWiden = option(b.widen)
      val optionAdd = if (above.isEmpty) None else option(b.add(above.head))

      val choice = List(optionNoop, optionWiden, optionAdd).maxBy(_.map(-_._1)).get
      if (optionAdd.isDefined && (choice eq optionAdd.get))
        (choice._2, above.tail)
      else (choice._2, above)

    }

    def optimize(b: Block, above: List[Block], nextDate: Option[Date]): (Block, List[Block]) = {
      val (newB, newOther) = optimizeOnce(b, above, nextDate)
      if (b eq newB) (newB, newOther)
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
        zipRows(makeBlocks(hd), sub)
    }
    loop(sortedCats)
  }


}
