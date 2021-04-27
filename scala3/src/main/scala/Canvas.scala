package pw.aldum.traycer

import scala.collection.immutable.Vector

case class Canvas(pixels: Vector[Vector[Color]]):
  def height = this.pixels.size
  def width = this.pixels(0).size

  override def toString = s"canvas($height, $width)"

  def apply(w: Int, h: Int) = new Canvas(w, h)

  def this(w: Int, h: Int, c: Color = Color.empty) = this(
    pixels = Vector.fill(h)(
      Vector.fill(w)(c)
    )
  )

  /** Return a new Canvas with the pixel changed at the specified index.
   *  If you provide indices out of bounds, this is a noop
   */
  def writePixel(x: Int, y: Int, color: Color): Canvas =
    if (x < width && x >= 0 &&
        y < height && y >= 0)
    then
      val r = pixels(y)
      val upd: Vector[Vector[Color]] = pixels.updated(y, r.updated(x, color))
      Canvas(upd)
    else this

  def getPixelAt(x: Int, y: Int): Color =
    pixels(y)(x)

  object PPM:
    def createHeader: String =
      s"""|P3
          |$width $height
          |255""".stripMargin

    def toPPM: String =
      val colArray = pixels.reverse.map(r =>
          r.grouped(5)
            .map(g =>
              g.map(
                p => p.toPPM
              ).mkString(" ")
          ).mkString("\n")
        )
        .mkString("\n")
      s"""|${createHeader}
      |$colArray
      |\n""".stripMargin

  end PPM

end Canvas
