import java.util.Date
import math._
import DateConversion._

object Block {
  def apply(area: Double, item: Item): Block = Block(List(), 0, area, item)
  private var indent = ""
}
case class Block private(private val _top: List[Block], spacer: Int, area: Double, item: Item) {
  def top = _top.reverse
  val width: Int = max(1, _top.map(_.width).sum) + spacer
  def idealWidth(targetRatio: Double) = sqrt(area / targetRatio)
  val height: Double = area / width
  val ratio: Double = height / width
  val firstDate: Date = (item.date :: _top.map(_.firstDate)).min

  def add(other: Block) = this.copy(_top = other :: _top)
  def widen = this.copy(spacer=spacer+1)

  override def toString = {
    val s = if (spacer > 0) " / %d".format(spacer) else ""
//    val a = "%s(%d, %.2f x %d%s, $%.2f, [".format(item.category.head.take(3), item.date.getDate, height, width, s, item.amt)
    val a = "%s(%d %.1f, $%.2f, [".format(item.category.head.take(3), item.date.getDate, width - idealWidth(Layout.targetRatio), item.amt)
    val prev = Block.indent
    Block.indent = Block.indent + (" " * a.size)
    val b = top.map(_.toString).mkString("\n"+Block.indent)
    Block.indent = prev
    val c = "])"
    a + b + c
  }
}

