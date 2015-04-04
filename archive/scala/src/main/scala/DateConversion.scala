import java.util.Date

object DateConversion {
  implicit def date_toLong(d: Date): Long = d.getTime
  implicit def long_toDate(d: Long): Date = new Date(d)
}
