

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
    val nrOfPixels = Math.pow((2 * radius) + 1, 2).toInt
    //    var i = x - radius
    //    var j = y - radius
    //    var redV, greenV, blueV, alphaV = 0
    //    while (i <= x + radius) {
    //      while (j <= y + radius) {
    //        val rgba = src(i, j)
    //        redV += red(rgba)
    //        greenV += green(rgba)
    //        blueV += blue(rgba)
    //        alphaV += alpha(rgba)
    //        j += 1
    //      }
    //      i += 1
    //      j = y - radius
    //    }

    val summedChannelValues = (for {
      i <- x - radius to x + radius
      j <- y - radius to y + radius
      srcRgba = src(clamp(i, 0, src.width), clamp(j, 0, src.height))
    } yield (red(srcRgba), green(srcRgba), blue(srcRgba), alpha(srcRgba)))
      .fold((0, 0, 0, 0))((sum, rgba) => (sum._1 + rgba._1, sum._2 + rgba._2, sum._3 + rgba._3, sum._4 + rgba._4))

    rgba(summedChannelValues._1 / nrOfPixels, summedChannelValues._2 / nrOfPixels, summedChannelValues._3 / nrOfPixels, summedChannelValues._4 / nrOfPixels)
  }
}
