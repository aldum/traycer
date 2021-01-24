package pw.aldum.traycer

case class Color(r: Double, g: Double, b: Double):
  override def toString = s"rgb($r, $g, $b)"

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
