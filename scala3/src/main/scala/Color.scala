package pw.aldum.traycer

case class Color(r: Double, g: Double, b: Double):
  override def toString = s"rgb($r, $g, $b)"

  def toPPM: String =
    def clamp(i: Int): Int =
        if (i < 0) 0
        else if (i > 255) 255
        else i
    val r255 = clamp( (256 * r).toInt )
    val g255 = clamp( (256 * g).toInt )
    val b255 = clamp( (256 * b).toInt )
    s"$r255 $g255 $b255"

//   def toPPM =
    // toColor255

  def +(that: Color) =
      Color( r + that.r
           , g + that.g
           , b + that.b)
  def -(that: Color) =
      Color( r - that.r
           , g - that.g
           , b - that.b)

  def *(sc: Double) =
      Color( r * sc
           , g * sc
           , b * sc)

  def *(that: Color): Color =
      Color( r * that.r
           , g * that.g
           , b * that.b)

  def ==(that: Color): Boolean =
      Seq( Math.abs(r - that.r)
         , Math.abs(g - that.g)
         , Math.abs(b - that.b)).forall(_ < epsilon)

end Color

case object Color:
  // let's say the default is pitch black
  def empty = Color(0, 0, 0)
