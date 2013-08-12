import java.io.File
import java.util.Date
import scala.math._
import scala.collection.SortedMap
import DateConversion._

object Main {
  def groupByMonth(items: List[Item]): SortedMap[Date, List[Item]] = {
    def month(i: Item) = new Date(i.date.getYear, i.date.getMonth, 1)
    SortedMap(items.groupBy(month).toList : _*)
  }

  def groupByCategory(items: List[Item]): (List[Category], List[Category]) = {
    val categories =
      for((cat, lst) <- items.groupBy(_.category.head).toList) yield Category(cat, lst)

    val (expenses, incomes) = categories.partition { c => c.expenses > c.income }
    (expenses, incomes)
//    (expenses.map(_.absorbIncomeIntoExpenses),
//     incomes.map(_.absorbExpensesIntoIncome))
  }


  def main(args: Array[String]) {
    val homeDir = System.getenv("HOME")
    val dir = new File(homeDir, "Dropbox/YNAB/Exports")
    val files = dir.listFiles().filter(_.getName.matches(".*-Register\\.csv")).sortBy(_.lastModified())
    val data = groupByMonth(Protocol.read(files.last))

    val june = data.toList(data.size-3)._2
    val (expenses, incomes) = groupByCategory(june.filter(_.amt > 0))
    val result = Layout(expenses)
    println("DONE")
    result.foreach(println)


  }

}
