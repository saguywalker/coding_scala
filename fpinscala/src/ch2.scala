sealed trait List[+A]
case object Nil extends List[Nothing]
case class Cons[+A](head: A, tail: List[A]) extends List[A]

object List{
  def main(args: Array[String]): Unit = {
    val xs = List(1,2,3,4,5,6,7,8)
    println(init(xs))
  }

  def sum(ints: List[Int]): Int = ints match{
    case Nil => 0
    case Cons(x, xs) => x + sum(xs)
  }

  def product(ds: List[Double]): Double = ds match {
    case Nil => 1.0
    case Cons(0.0, _) => 0.0
    case Cons(x, xs) => x * product(xs)
  }

  def tail[A](xs: List[A]): List[A] = xs match {
    case Nil => Nil
    case Cons(_, Nil) => Nil
    case Cons(_, ys) => ys
  }

  def setHead[A](xs: List[A], a: A): List[A] = {
    Cons(a, tail(xs))
  }

  def apply[A](as: A*): List[A] ={
    if (as.isEmpty) Nil
    else Cons(as.head, apply(as.tail: _*))
  }

  def drop[A](l: List[A], n: Int): List[A] = {
    if (n <= 0) l
    else drop(tail(l), n - 1)
  }

  def dropWhile[A](l: List[A], f: A => Boolean): List[A] = {
    l match {
      case Cons(h, t) if f(h) => dropWhile(t, f)
      case _ => l
    }
  }

  def init[A](l: List[A]): List[A] = l match{
    case Cons(_, Nil) => Nil
    case Nil => Nil
    case Cons(h, t) => Cons(h, init(t))
  }
}