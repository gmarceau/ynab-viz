import cc.spray.json._
import java.text.SimpleDateFormat
import java.util.Date

object Protocol extends cc.spray.json.DefaultJsonProtocol {
  implicit val dateFormat = new JsonFormat[Date] {
    val dateFormatString = "MM/dd/yyyy"
    def write(d: Date) = new SimpleDateFormat(dateFormatString).format(d).toJson
    def read(value: JsValue) = value match {
      case JsString(str) => new SimpleDateFormat(dateFormatString).parse(str)
      case _ => throw new DeserializationException("Cannot turn JsValue into a Date: " + value)
  } }
}
