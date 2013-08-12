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
  val height: Double = area / width
  val ratio: Double = height / width
  val weightedDate: Date = {
    val sumArea = _top.map(_.area).sum + area
    val here = item.date * area / sumArea
    val up = (for(b <- _top) yield b.weightedDate * b.area / sumArea).sum
    (here + up).toLong
  }

  def add(other: Block) = this.copy(_top = other :: _top)
  def widen = this.copy(spacer=spacer+1)

  override def toString = {
    val s = if (spacer > 0) " / %d".format(spacer) else ""
    val a = "%s(%.2f x %d%s, $%.2f, [".format(item.category.head, height, width, s, item.amt)
    val prev = Block.indent
    Block.indent = Block.indent + (" " * a.size)
    val b = top.map(_.toString).mkString("\n"+Block.indent)
    Block.indent = prev
    val c = "])"
    a + b + c
  }
}

