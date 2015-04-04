import java.awt.Color


class Block {
  val w: Double
  val h: Double
}

case class ColorBlock(w: Double, h: Double, c: Color) extends Block

case class Subblock(dx: Double, dy: Double, b: Block)
case class GroupBlock(w: Double, h: Double, subblocks: List[Subblock]) extends Block {
  def hAppend(others: Block*) = {
    val rw = this.w + others.map(_.w).sum
    val rh = this.h + others.map(_.h).sum

    var x = 0
    for(b <- this :: others.toList)
    yield Subblock(x, 0, b)

    GroupBlock(rw, rh, subblocks)
  }
}
