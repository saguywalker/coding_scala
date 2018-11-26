/*
bisect.scala

 */

object Bisect {

  def findRoot(low: Double, high: Double)(f: Double => Double): Option[Double] = {
    @annotation.tailrec
    def find(low: Double, high: Double): Option[Double] = {
      val flow = f(low)
      val fhigh = f(high)

      if(flow * fhigh >= 0) None

      val mid = (low + high) / 2
      val fmid = f(mid)

      if(fmid == 0) Some(mid)
      else if(fmid * flow < 0) find(low, mid)
      else find(mid, high)
    }

    find(low, high)
  }

  def main(args: Array[String]): Unit = {
    println(findRoot(-10 , 10)(x => x + 1))
    println(findRoot(-5.0,10.0)(x => 2.0-x))
    println(findRoot(0.0,5.0)(x => x-1.0))
    println(findRoot(0.0,2.0)(x => (x+1.0)*(x-1.0)))
    println(findRoot(-2.0,0.0)(x => (x+1.0)*(x-1.0)))
  }


}

/* eof */

