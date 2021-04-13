// case object lol:

import pw.aldum.traycer.Geometry._
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

val can = new Canvas(200, 100)

def animate(p: Projectile): Projectile =
  // println(p)
  val x = p.pos.x.toInt
  val y = p.pos.y.toInt
  can.safeWritePixel(x , y, Color(0, 0, 1))
  if (p.pos.y > 0) animate( tick(e, p) )
  else p

animate(p)


def write =
  val path = "/tmp/output.ppm" // sys.env("OUTPUT_PATH")
  val printWriter = new java.io.PrintWriter(path)
  printWriter.println(can.PPM.toPPM)
  printWriter.close()


Float.MaxValue
Double.MaxValue
