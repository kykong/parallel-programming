package reductions

import scala.annotation._
import org.scalameter._
import common._

object ParallelParenthesesBalancingRunner {

  @volatile var seqResult = false

  @volatile var parResult = false

  val standardConfig = config(
    Key.exec.minWarmupRuns -> 40,
    Key.exec.maxWarmupRuns -> 80,
    Key.exec.benchRuns -> 120,
    Key.verbose -> true
  ) withWarmer(new Warmer.Default)

  def main(args: Array[String]): Unit = {
    val length = 100000000
    val chars = new Array[Char](length)
    val threshold = 10000
    val seqtime = standardConfig measure {
      seqResult = ParallelParenthesesBalancing.balance(chars)
    }
    println(s"sequential result = $seqResult")
    println(s"sequential balancing time: $seqtime ms")

    val fjtime = standardConfig measure {
      parResult = ParallelParenthesesBalancing.parBalance(chars, threshold)
    }
    println(s"parallel result = $parResult")
    println(s"parallel balancing time: $fjtime ms")
    println(s"speedup: ${seqtime / fjtime}")
  }
}

object ParallelParenthesesBalancing {

  /** Returns `true` iff the parentheses in the input `chars` are balanced.
   */
//  def balance(chars: Array[Char]): Boolean = {
//    if (chars.isEmpty) true
//    
//    else if (chars.size == 1) {
//      val ch = chars(0)
//      ch != '(' && ch != ')'
//    }
//    
//    else {
//      var res = true
//      var count = 0
//      for (i <- 0 until chars.size) {
//        val ch = chars(i)
//        if (ch == '(') count += 1
//        else if (ch == ')') {
//          count -= 1
//          if (count < 0) res = false
//        }
//      }
//      if (count != 0) res = false
//      res
//    }
//  }

  
  def balance(chars: Array[Char]): Boolean = {
    
    
    def balanceRecurse(chars: Array[Char], from: Int, to: Int, accumulator: Int): Boolean = {
      if (chars.isEmpty) true

      else if (chars.size == 1) {
        val ch = chars(0)
        ch != '(' && ch != ')'
      }

      else if (accumulator < 0) false
      
      else if (from > to) (accumulator == 0)
      
      else {
        val ch = chars(from)
        if (ch == '(') balanceRecurse(chars, from + 1, to, accumulator + 1)
        else if (ch == ')') balanceRecurse(chars, from + 1, to, accumulator - 1)
        else balanceRecurse(chars, from + 1, to, accumulator)
      }
    }
    
    balanceRecurse(chars, 0, chars.length - 1, 0)
  }
  
  
//  def balance(chars: Array[Char]): Boolean = {
//    val interm = chars.map ( _ match { 
//          case '(' => 1
//          case ')' => -1
//          case _ => 0
//        }
//      )
//      
//      val total = interm.foldLeft(0)(_ + _)
//      val neg = interm.scanLeft(0)(_ + _).filter { x => x < 0 }
//
//      if (interm.length == 0) return true      
//      if (!neg.isEmpty) return false;
//      return total == 0
//  }
  
  
  /** Returns `true` iff the parentheses in the input `chars` are balanced.
   */
  def parBalance(chars: Array[Char], threshold: Int): Boolean = {

    def traverse(idx: Int, until: Int, arg1: Int, arg2: Int): (Int, Array[Int]) = {

      var acc = 0
      var i = idx
      var arr = scala.collection.mutable.ArrayBuffer[Int]()
      //var set = Set[Int]()
      
      while (i < until) {
        val ch = chars(i)
        if (ch == '(') acc += 1
        else if (ch == ')') acc -= 1
        
        //if (acc < 0) set = set + acc
        if (acc < 0) arr += acc
        i = i + 1
      }
      
//      var setStr = ""
//      set.foreach { x => setStr += (x + " ") }
      
      
      //println(s"traverse: $idx until $until ($acc, [$setStr])")
      //(acc, set.toArray)
      (acc, arr.toArray)
    }

    def reduce(from: Int, until: Int): (Int, Array[Int]) = {
      val mid = from + ((until - from) / 2)
      //println(s"Mid = $mid")
      
      if ((until - from) <= threshold) {
        //println(s"Traversing from $from until $until")
        traverse(from, until, 0, 0)
        
      } else {
        //println(s"reducing :  ($from, $mid) and ($mid, $until)")
        val (l, r) = parallel(reduce(from, mid),
                              reduce(mid, until))

        //println(s"reduce results:  ($l, $r)")
        val l_total = l._1
        val l_list = l._2
        
        val r_list =  r._2.map { x => x + l_total }.filter { x => x < 0 }
        
        //val lstStr = printArrayContents(l_list ++ r_list)
        //val totalStr = (l_total + r._1).toString()
        
        //println(s"Returning ($totalStr, [$lstStr])")
        
        (l_total + r._1, l_list ++ r_list)
      }
    }
    
    def printArrayContents(in: Array[Int]): String = {
      var out = ""
      in.foreach { x => out += x + " " }
      out
    }

    //val str = new String(chars)
    //println(s"Balancing for $str")
    //reduce(0, chars.length) == (0, Array.empty[Int])
    val (total, arr) = reduce(0, chars.length)
    
    total == 0 && arr.isEmpty
    
  }

  // For those who want more:
  // Prove that your reduction operator is associative!

}
