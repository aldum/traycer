import pw.aldum.traycer.Geometry.*
import pw.aldum.traycer.Canvas
import pw.aldum.traycer.Color

case class Projectile(pos: Vec4D, vel: Vec4D):
  override def toString = s"at (${pos.x}, ${pos.y}, ${pos.z})"

case class Environment(gravity: Vec4D, wind: Vec4D)

def tick(env: Environment, p: Projectile): Projectile =
  Projectile( p.pos + p.vel
            , p.vel + env.gravity + env.wind)

val vel = Vector(4.5, 4, 0)
val p = Projectile( Point(0, 1, 0)
                  , vel )

val e = Environment( Vector(0, -0.1, 0)
                   , Vector(-0.01, 0, 0))

val initCan = new Canvas(200, 100)

def animate(p: Projectile, can: Canvas): Projectile =
  // println(p)
  val x = p.pos.x.toInt
  val y = p.pos.y.toInt
  val outCan = can.writePixel(x, y, Color(0, 0, 1))
  write(outCan)
  if (p.pos.y > 0) animate( tick(e, p), outCan )
  else p

// animate(p, initCan)


def write(canvas: Canvas, path: String = "/tmp/output.ppm") =
  val printWriter = new java.io.PrintWriter(path)
  printWriter.println(canvas.PPM.toPPM)
  printWriter.close()


Float.MaxValue
Double.MaxValue


var clockCan = new Canvas(200, 200)
def drawClock =
  import Transformation.*
  for i <- Range(0, 12)
  do
    val p = Point(50, 0, 0)
    val r = Math.PI / 6
    val pp = (Translate(Vector(100, 100, 0)) * Rotate(Base.Z, i * r) * p).get
    // println(s"${pp.col(0)(0)}, ${pp.col(0)(1)}")
    val x = pp.col(0)(0).toInt
    val y = pp.col(0)(1).toInt
    clockCan = clockCan.writePixel(x, y, Color(0.5, 0.5, 1))

drawClock
write(clockCan, "/tmp/clock.ppm")


