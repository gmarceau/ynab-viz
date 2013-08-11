import java.io.File
import java.util.Date
import scala.math._
import DataConversion._

case class Item(date: Date, category: List[String], payee: String, memo: String, outflow: Double, inflow: Double) {
  def label: String = {
    val skipList = List("DBT CRD", "POS DEB", "PPD", "Payroll", "Paypal", "PAYPAL", "Payee",
                        "T MOBILE", "ATM FEE", "AMAZON", "Amazon", "Check ", "CREDIT", "DDA", "DEBIT", "e-Pay", "Interest Deposit",
                        "KEY FOODS", "FOODTOWN", "Adjustment", "MTA", "Transfer", "USPS").map(_.r)
    def skip(str: String): Boolean = skipList.find(_.findFirstIn(str).isDefined).isDefined
    List(memo, payee, category.last).find(!skip(_)).get
  }
  def amt = outflow - inflow

}
case class Category(name: String, items: List[Item]) {
  def expenses = items.filter(_.amt > 0).map(_.amt).sum
  def income = -items.filter(_.amt < 0).map(_.amt).sum
  def absorbIncomeIntoExpenses = {
    val ratio = (expenses - income) / expenses
    val result = for (i <- items) yield { i.copy(outflow = i.outflow * ratio, inflow = 0) }
    this.copy(items = result)
  }
}

object Block {
  def apply(area: Double, item: Item) = Block(List(), 0, area, item)
}
case class Block private(private val _top: List[Block], spacer: Int, area: Double, item: Item) {
  def top = _top.reverse
  val width: Int = max(1, _top.map(_.width).sum + spacer)
  val height: Double = area / width
  val ratio: Double = height / width
  val weightedDate: Date = {
    val sumArea = _top.map(_.area).sum + area
    val here = item.date * area / sumArea
    val up = (for(b <- _top) yield b.weightedDate * b.area / sumArea).sum
    (here + up).toLong
  }

  def add(other: Block) = this.copy(_top = other :: _top)
  def widen = this.copy(spacer=spacer+1)
}

object Main {
  val millisecondsPerDay = 1000d * 60 * 60 * 24
  private val targetRatio = 1 / 1.61803398875

  object fitnessWeights {
    val targetRatio = 1d
    val dateAlignment = 1d * (1 / millisecondsPerDay)
  }


  def zipRows(widthPerDay: Double, here: List[Block], above: List[Block]): List[Block] = {

    def score(b: Block, nextDate: Option[Date]): Double = {

      val x = fitnessWeights.targetRatio * abs(b.ratio - targetRatio)
      val y = nextDate match { case Some(d) =>
                                  val blockRightEdgeDate = b.weightedDate + b.width / widthPerDay
                                  fitnessWeights.dateAlignment * max(0, blockRightEdgeDate - d)
                               case None => 0
                             }
      x + y
    }

    def optimizeOnce(b: Block, above: List[Block], nextDate: Option[Date]): (Block, List[Block]) = {
      def option(candidate: Block) = Some(score(candidate, nextDate) -> candidate)
      val optionNoop = option(b)
      val optionWiden = option(b.widen)
      val optionAdd = if (above.isEmpty) None else option(b.add(above.head))

      val choice = List(optionNoop, optionWiden, optionAdd).minBy(_.map(_._1)).get
      if (choice eq optionAdd)
        (choice._2, above.tail)
      else (choice._2, above)

    }

    def optimize(b: Block, above: List[Block], nextDate: Option[Date]): (Block, List[Block]) = {
      val (newB, newOther) = optimizeOnce(b, above, nextDate)
      if (b eq newB) (newB, newOther)
      else optimize(newB, newOther, nextDate)
    }

    def loop(here: List[Block], above: List[Block]): List[Block] = here match {
      case Nil => List()
      case here1 :: Nil =>
        List(above.foldLeft(here1) { _.add(_) })
      case here1 :: here2 :: tail =>
        val (b, restAbove) = optimize(here1, above, Some(here2.item.date))
        b :: loop(here, restAbove)
    }

    loop(here, above)
  }

  def main(args: Array[String]) {
    val homeDir = System.getenv("HOME")
    val dir = new File(homeDir, "Dropbox/YNAB/Exports")
    val files = dir.listFiles().filter(_.getName.matches(".*-Register\\.csv")).sortBy(_.lastModified())
    val (expenses, incomes) = Protocol.read(files.last)
                                       .map(_.absorbIncomeIntoExpenses)
                                       .partition { c => c.expenses > c.income }
    expenses.foreach(println)
    println()
    incomes.foreach(println)
  }

}
