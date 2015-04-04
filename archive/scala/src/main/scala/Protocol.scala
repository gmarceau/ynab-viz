
import collection.mutable.{ Map => MMap }
import java.io.{FileReader, File}
import java.text.SimpleDateFormat
import java.util.Date

case class Cell(name: String, month: String, v: Double)
case class Month(name: String, income: Double, expenses: List[Cell], savings: Double) {
  def categories = expenses.map(_.name)
  def expenses(name: String): Cell = expenses.find(_.name == name).get
}


object Protocol {
  val dateFormatString = "MM/dd/yyyy"
  def write(d: Date) = new SimpleDateFormat(dateFormatString).format(d)
  def read(str: String) = new SimpleDateFormat(dateFormatString).parse(str)

  def parseCell(s: String) = {
    val noQuotes = "\"(.*)\"".r.findFirstMatchIn(s) match {
      case None => s
      case Some(m) => m.group(1)
    }
    val noDollar = noQuotes.replaceAllLiterally("$", "")
    noDollar
  }

  type Row = List[String]
  
  def parseRow(s: String): Row = s.split(",").map(parseCell).toList

  def parseHeader(s: Row) = s.tail.take(s.size - 4)

  def isSpacer(s: Row) = s.tail.forall(_.isEmpty)

  def splitAtSpacers(s: List[Row]): List[List[Row]] = {
    if (s.isEmpty) List()
    else {
      val (here, rest) = s.tail.span(!isSpacer(_))
      (s.head :: here) :: splitAtSpacers(rest)
    }
  }

  def parseCategory(months: List[String], name: String, s: List[Row]): List[Cell] =
    for((c, m) <- s.last.tail.zip(months)) yield Cell(name, m, c.toDouble)

  def parseCategory(months: List[String], s: List[Row]): List[Cell] = parseCategory(months, s.head.head, s.tail)

  def parseMonths(months: List[String], income: List[Cell], expenses: List[List[Cell]]) = {
    for(((m, i), e) <- months.zip(income).zip(expenses))
      yield Month(m, i.v, e, i.v - e.map(_.v).sum)
  }

  def parse(s: List[String]) = {
    val rows = s.map(parseRow)
    val months = parseHeader(rows.head)
    val sections = splitAtSpacers(rows.tail)
    val _ :: i :: u :: others = sections
    val income = parseCategory(months, "income", i.tail)
    val uncategorized = parseCategory(months, "uncategorized", u.tail)
    val expenses =
       others.dropRight(1).map(parseCategory(months, _)) ++
         List(parseCategory(months, others.last.dropRight(2)))
    parseMonths(months, income, (uncategorized :: expenses).transpose)
  }


}
