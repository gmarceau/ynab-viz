import java.util.Date

object DataConversion {
  implicit def date_toLong(d: Date): Long = d.getTime
  implicit def long_toDate(d: Long): Date = new Date(d)
}
