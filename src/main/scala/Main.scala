import java.io.File
import java.util.Date
import scala.collection.SortedMap
import org.apache.commons.io.FileUtils


object Main {
  def groupByMonth(items: List[Item]): SortedMap[Date, List[Item]] = {
    def month(i: Item) = new Date(i.date.getYear, i.date.getMonth, 1)
    SortedMap(items.groupBy(month).toList : _*)
  }

  def groupByCategory(items: List[Item]): (List[Category], List[Category]) = {
    val categories =
      for((cat, lst) <- items.groupBy(_.category.head).toList) yield Category(cat, lst)

    val (expenses, incomes) = categories.partition { c => c.expenses > c.income }
//    (expenses, incomes)
    (expenses.map(_.absorbIncomeIntoExpenses),
     incomes.map(_.absorbExpensesIntoIncome))
  }

  def layoutMonth(items: List[Item]) = {
    val (expenses, incomes) = groupByCategory(items)
//    Layout(expenses)
//    Layout(incomes)
    Layout(expenses)
  }


  def main(args: Array[String]) {
    val homeDir = System.getenv("HOME")
    val dir = new File(homeDir, "Dropbox/YNAB/Exports")
    val files = dir.listFiles().filter(_.getName.matches(".*-Register\\.csv")).sortBy(_.lastModified())
    val data = groupByMonth(Protocol.read(files.last))

    val month = data.toList(data.size-2)._2
    val result = layoutMonth(month)
    println("DONE " + new File(".").getCanonicalPath)
    FileUtils.writeStringToFile(new File("output.log"), result.map(_.toString).mkString("\n"))
    FileUtils.writeStringToFile(new File("output.ss"), result.map(_.toScheme).mkString("(", "\n", ")"))
  }

}
