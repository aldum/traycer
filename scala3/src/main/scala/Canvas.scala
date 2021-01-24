package pw.aldum.traycer

case class Canvas( pixels: Array[Array[Color]] ):
  def height = this.pixels(0).size
  def width = this.pixels.size

  override def toString = s"canvas($height, $width)"

  def apply(w: Int, h: Int) = new Canvas(w, h)

  def this(w: Int, h: Int) = this(
    pixels = Array.fill(w)(
      Array.fill(h)(Color.empty)
    )
  )

  def writePixel(x: Int, y: Int, color: Color): Canvas =
    pixels(x)(y) = color
    this

  def pixelAt(x: Int, y: Int): Color =
    pixels(x)(y)

end Canvas

// case object Canvas:

// class Canvas:
//   val pixels: Array[Array[Color]] = Array.fill(width)(
//       Array.fill(height)(Color.empty)
//     )

//   val height: Int
//   val width: Int

//   def Canvas(w: Int, h: Int): Canvas =
//     this.height = h
//     this.width = w
