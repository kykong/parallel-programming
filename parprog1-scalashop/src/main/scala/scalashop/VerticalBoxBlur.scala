package scalashop

import org.scalameter._
import common._

object VerticalBoxBlurRunner {

  val standardConfig = config(
    Key.exec.minWarmupRuns -> 5,
    Key.exec.maxWarmupRuns -> 10,
    Key.exec.benchRuns -> 10,
    Key.verbose -> true
  ) withWarmer(new Warmer.Default)

  def main(args: Array[String]): Unit = {
    val radius = 3
    val width = 1920
    val height = 1080
    val src = new Img(width, height)
    val dst = new Img(width, height)
    val seqtime = standardConfig measure {
      VerticalBoxBlur.blur(src, dst, 0, width, radius)
    }
    println(s"sequential blur time: $seqtime ms")

    val numTasks = 32
    val partime = standardConfig measure {
      VerticalBoxBlur.parBlur(src, dst, numTasks, radius)
    }
    println(s"fork/join blur time: $partime ms")
    println(s"speedup: ${seqtime / partime}")
  }

}

/** A simple, trivially parallelizable computation. */
object VerticalBoxBlur {

  /** Blurs the columns of the source image `src` into the destination image
   *  `dst`, starting with `from` and ending with `end` (non-inclusive).
   *
   *  Within each column, `blur` traverses the pixels by going from top to
   *  bottom.
   */
  def blur(src: Img, dst: Img, from: Int, end: Int, radius: Int): Unit = {
    // TODO implement this method using the `boxBlurKernel` method
    
     for (x <- from until end) {
      for (y <- 0 until src.height) {
        //println(s"Processing ($x, $y)")
        val result = boxBlurKernel(src, x, y, radius)
        dst.update(x, y, result)
        //println(s"Copied $result to dst($x, $y)")
      }
    }
    
    
  }

  /** Blurs the columns of the source image in parallel using `numTasks` tasks.
   *
   *  Parallelization is done by stripping the source image `src` into
   *  `numTasks` separate strips, where each strip is composed of some number of
   *  columns.
   */
  def parBlur(src: Img, dst: Img, numTasks: Int, radius: Int): Unit = {
    // TODO implement using the `task` construct and the `blur` method
    //println("Vertical parblur")
    val width = src.width
    
    var step = width / numTasks
    if (step == 0) step = 1
    
    
    val startIndices = (0 to width by step).toList
    //println(startIndices)
    //val endIndices = startIndices.tail :+ (src.width + 1)
    val endIndices = startIndices.tail
    val lastElement = startIndices.last
    var strips = startIndices.dropRight(1) zip endIndices
    
    if (lastElement < width) {
      strips = strips :+ (lastElement, width)
    }
    
    //println(strips)
    
    val tasks = strips map {case (from: Int, end: Int) => task { blur(src, dst, from, end, radius)}}
    //println(tasks)
    
    val task1 = tasks.head
    
    for (subtask <- tasks) subtask.join()
    //task1
    
  }

}
