import collection.mutable.{Map => MMap, ListBuffer}
import org.supercsv.prefs.CsvPreference
import scala.io.Source
import java.io.{File, FileReader}
import org.supercsv.io._
import cc.spray.json._
import JsonTransforms._
import scala.collection.JavaConversions._
import java.util.Date


case class Item(date: Date, category: List[String], payee: String, memo: String, outflow: Double, inflow: Double)

object Main {
  import Protocol._
  implicit val itemFormat = jsonFormat6(Item)

  def asImmutable[A, B](m: MMap[A, B]) = Map(m.toList : _*)
  def isTransfer(i: Item) = i.category == List()
  def read(f: File) = {
    val reader = new CsvMapReader(new FileReader(f), CsvPreference.STANDARD_PREFERENCE)
    val headers = reader.getHeader(true)
    val maps = Stream.continually(reader.read(headers:_*)).takeWhile(_ != null).toList
    val jsons =
      for(m <- maps) yield
        asImmutable(m).toJson.rename("Date", "date")
                             .rename("Category", "category")
                             .transform("category", (str:String) => if (str==null) Array[String]() else str.split(":"))
                             .rename("Memo", "memo").transform("memo", (str:String) => if (str==null) "" else str)
                             .rename("Payee", "payee").transform("payee", (str:String) => if (str==null) "" else str)
                             .rename("Outflow", "outflow").transform("outflow", (str:String) => str.replace("$", "").toDouble)
                             .rename("Inflow", "inflow").transform("inflow", (str:String) => str.replace("$", "").toDouble)

    jsons.map(_.convertTo[Item]).filterNot(isTransfer(_)).sortBy(_.date)
  }

  def main(args: Array[String]) {
    val homeDir = System.getenv("HOME")
    val dir = new File(homeDir, "Dropbox/YNAB/Exports")
    val files = dir.listFiles().filter(_.getName.matches(".*-Register\\.csv")).sortBy(_.lastModified())
    val data = read(files.last)
    data.foreach(println)
  }

}
