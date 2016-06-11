
import common._

package object scalashop {

  /** The value of every pixel is represented as a 32 bit integer. */
  type RGBA = Int

  /** Returns the red component. */
  def red(c: RGBA): Int = (0xff000000 & c) >>> 24

  /** Returns the green component. */
  def green(c: RGBA): Int = (0x00ff0000 & c) >>> 16

  /** Returns the blue component. */
  def blue(c: RGBA): Int = (0x0000ff00 & c) >>> 8

  /** Returns the alpha component. */
  def alpha(c: RGBA): Int = (0x000000ff & c) >>> 0

  /** Used to create an RGBA value from separate components. */
  def rgba(r: Int, g: Int, b: Int, a: Int): RGBA = {
    (r << 24) | (g << 16) | (b << 8) | (a << 0)
  }

  /** Restricts the integer into the specified range. */
  def clamp(v: Int, min: Int, max: Int): Int = {
    if (v < min) min
    else if (v > max) max
    else v
  }

  /** Image is a two-dimensional matrix of pixel values. */
  class Img(val width: Int, val height: Int, private val data: Array[RGBA]) {
    def this(w: Int, h: Int) = this(w, h, new Array(w * h))
    def apply(x: Int, y: Int): RGBA = data(y * width + x)
    def update(x: Int, y: Int, c: RGBA): Unit = data(y * width + x) = c
  }

  /** Computes the blurred RGBA value of a single pixel of the input image. */
  def boxBlurKernel(src: Img, x: Int, y: Int, radius: Int): RGBA = {
    // TODO implement using while loops
    var origin = src(x, y)
    //println(s"Origin ($x, $y) = $origin, radius = $radius")
    if (radius==0) 
      origin
      
    else {
      //var p = x - radius
      //var q = clamp(y - radius, 0, src.height)
      var q = clamp(y - radius, 0, y - radius)
      val pMax = clamp(x + radius, x + radius, src.width - 1)
      val qMax = clamp(y + radius, y + radius, src.height - 1)
      
      //println(s"Clamped q = $q; pMax = $pMax; qMax = $qMax")
      
      var count = 0
      var redTotal, greenTotal, blueTotal, alphaTotal = 0
      
      while (q <= qMax) {
        var p = clamp(x - radius, 0, x - radius)
        //println(s"Clamped p = $p")
          while (p <= pMax) {
            //println(s"Evaluating ($p, $q)")
            //if (!(p == x && q == y)) {
              count += 1
              val pixel = src(p, q)

              redTotal += red(pixel)
              greenTotal += green(pixel)
              blueTotal += blue(pixel)
              alphaTotal += alpha(pixel)
              //println(s"Calculated for ($p, $q).  Alpha Total = $alphaTotal")
            //}
            p += 1
        }
        q += 1
      }
      //println(s"redTotal: $redTotal; greenTotal: $greenTotal; blueTotal: $blueTotal; alphaTotal: $alphaTotal")
      //val result = rgba((redTotal.toDouble / count).toInt, greenTotal / count, blueTotal / count, alphaTotal / count)
      val result = rgba(divide(redTotal, count), divide(greenTotal, count), divide(blueTotal, count), divide(alphaTotal, count))
      //println(s"Count = $count")
      //println(s"Result = $result")
      result
    }
  }

   //def divide(a: Int, b: Int): Int = math.ceil(a.toDouble / b).toInt
   //def divide(a: Int, b: Int): Int = (a.toDouble / b).round.toInt
  def divide(a: Int, b: Int): Int = a / b
}
