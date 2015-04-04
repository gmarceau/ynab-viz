import java.io.File
import java.util.Date
import scala.collection.SortedMap
import org.apache.commons.io.FileUtils

import scala.io.Source
import org.jfree.graphics2d.svg.SVGGraphics2D

object Main {

  def main(args: Array[String]) {
    val homeDir = System.getenv("HOME")
    val dir = new File(homeDir, "Dropbox/YNAB")
    val file = dir.listFiles().filter(_.getName.matches(".*-Income " +
      "v\\. Expense.*\\.csv")).sortBy(_.lastModified()).last

    def average(cells: List[Cell]) = cells.map(_.v).sum / cells.size

    val months = Protocol.parse(Source.fromFile(file).getLines().toList)
    val averages = (for(c <- months.head.categories)
                    yield c -> average(months.map(_.expenses(c))))
                   .toMap

    val categoryOrder = averages.toList.sortBy(_._2).reverse.map(_._1)
    val categoryRows = {
      val List(a, b, c, d, rest @ _*) = categoryOrder
      List(List(a), List(b, c, d), rest)
    }


    println(averages)

    val g2 = new SVGGraphics2D(1000, 1000)





  }

}
