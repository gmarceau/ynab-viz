import cc.spray.json._
import collection.mutable.{ Map => MMap }
import java.io.{FileReader, File}
import java.text.SimpleDateFormat
import java.util.Date
import org.supercsv.io.CsvMapReader
import org.supercsv.prefs.CsvPreference
import cc.spray.json._
import JsonTransforms._
import scala.collection.JavaConversions._

object Protocol extends cc.spray.json.DefaultJsonProtocol {
  implicit val dateFormat = new JsonFormat[Date] {
    val dateFormatString = "MM/dd/yyyy"
    def write(d: Date) = new SimpleDateFormat(dateFormatString).format(d).toJson
    def read(value: JsValue) = value match {
      case JsString(str) => new SimpleDateFormat(dateFormatString).parse(str)
      case _ => throw new DeserializationException("Cannot turn JsValue into a Date: " + value)
  } }

  implicit val itemFormat = jsonFormat6(Item)

  def asImmutable[A, B](m: MMap[A, B]) = Map(m.toList : _*)
  def isTransfer(i: Item) = i.category == List()
  def read(f: File): List[Item] = {
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

}
