object MAS{

    import math.pow
    import math.sqrt

    def meanAndSD(x: Vector[Double]): (Double, Double) = {
        val mean = x.sum / x.size
        val sd = sqrt(x.map(i => pow((i - mean), 2)).sum / x.size)

        (mean, sd)
    }

    def main(args: Array[String]): Unit = {
        println(meanAndSD(Vector(600,470,170,430,300)))
    }
}