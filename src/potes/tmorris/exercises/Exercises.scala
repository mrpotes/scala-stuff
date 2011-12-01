package potes.tmorris.exercises

//
// http://blog.tmorris.net/revised-scala-exercises/
//

sealed trait List[+A] {
  override def toString = {
    def toScalaList(t: List[A]): scala.List[A] = t match {
      case Empty => Nil
      case Cons(h, t) => h :: toScalaList(t)
    }
    toScalaList(this).toString
  }
}
final case object Empty extends List[Nothing]
final case class Cons[A](h: A, t: List[A]) extends List[A]

object List {
  def foldRight[A, B](as: List[A], b: B, f: (A, B) => B): B = as match {
    case Empty => b
    case Cons(h, t) => f(h, foldRight(t, b, f))
  }

  def foldLeft[A, B](as: List[A], b: B, f: (B, A) => B): B = as match {
    case Empty => b
    case Cons(h, t) => foldLeft(t, f(b, h), f)
  }

  def reduceRight[A](as: List[A], f: (A, A) => A): A = as match {
    case Empty => error("bzzt. reduceRight on empty list")
    case Cons(h, t) => foldRight(t, h, f)
  }

  def reduceLeft[A](as: List[A], f: (A, A) => A): A = as match {
    case Empty => error("bzzt. reduceLeft on empty list")
    case Cons(h, t) => foldLeft(t, h, f)
  }

  def unfold[A, B](b: B, f: B => Option[(A, B)]): List[A] = f(b) match {
    case Some((a, b)) => Cons(a, unfold(b, f))
    case scala.None => Empty
  }
}

sealed trait Natural {
  override def toString = {
    def toInt(n: Natural): Int = n match {
      case Zero => 0
      case Succ(x) => 1 + toInt(x)
    }
    toInt(this).toString
  }
}
final case object Zero extends Natural
final case class Succ(c: Natural) extends Natural

object Exercises {
  
  implicit def toList(ints : Array[Int]) : List[Int] = {
    val start : List[Int] = Empty
    ints.foldRight(start)( (x: Int, l : List[Int]) => Cons(x, l))
  }
  
  implicit def toNatural(x : Int) : Natural = {
    if (x == 0) Zero
    else Succ(toNatural(x - 1))
  }
  
  def main(args : Array[String]) : Unit = {
    println(toNatural(5))
    println(add(5, 3))
    println(toList(Array(1,2,3,4,5)))
    println(sum(Array(1,2,3,4,5)))
    println(length(Array(1,2,3,4,5)))
    println(map(Array(1,2,3,4,5), (a : Int) => a.toString + "i"))
  }

  // Exercise 1
  // Relative Difficulty: 1
  // Correctness: 2.0 marks
  // Performance: 0.5 mark
  // Elegance: 0.5 marks
  // Total: 3
  def add(x: Natural, y: Natural): Natural = x match {
      case Zero => y
      case Succ(i) => Succ(add(i, y))
  }

  // Exercise 2
  // Relative Difficulty: 2
  // Correctness: 2.5 marks
  // Performance: 1 mark
  // Elegance: 0.5 marks
  // Total: 4
  def sum(is: List[Int]): Int = List.foldLeft(is, 0, (b: Int, a: Int) => a + b)

  // Exercise 3
  // Relative Difficulty: 2
  // Correctness: 2.5 marks
  // Performance: 1 mark
  // Elegance: 0.5 marks
  // Total: 4
  def length[A](as: List[A]): Int = List.foldLeft(as, 0, (b: Int, a: A) => b + 1)

  // Exercise 4
  // Relative Difficulty: 5
  // Correctness: 4.5 marks
  // Performance: 1.0 mark
  // Elegance: 1.5 marks
  // Total: 7
  def map[A, B](as: List[A], f: A => B): List[B] = {
    List.foldRight(as, Empty, (a: A, b : List[B]) => Cons(f(a), b))
  }

  // Exercise 5
  // Relative Difficulty: 5
  // Correctness: 4.5 marks
  // Performance: 1.5 marks
  // Elegance: 1 mark
  // Total: 7
  def filter[A](as: List[A], f: A => Boolean): List[A] = error("todo")

  // Exercise 6
  // Relative Difficulty: 5
  // Correctness: 4.5 marks
  // Performance: 1.5 marks
  // Elegance: 1 mark
  // Total: 7
  def append[A](x: List[A], y: List[A]): List[A] = error("todo")

  // Exercise 7
  // Relative Difficulty: 5
  // Correctness: 4.5 marks
  // Performance: 1.5 marks
  // Elegance: 1 mark
  // Total: 7
  def flatten[A](as: List[List[A]]): List[A] = error("todo")

  // Exercise 8
  // Relative Difficulty: 7
  // Correctness: 5.0 marks
  // Performance: 1.5 marks
  // Elegance: 1.5 mark
  // Total: 8
  def flatMap[A, B](as: List[A], f: A => List[B]): List[B] = error("todo")

  // Exercise 9
  // Relative Difficulty: 8
  // Correctness: 3.5 marks
  // Performance: 3.0 marks
  // Elegance: 2.5 marks
  // Total: 9
  def maximum(is: List[Int]): Int = error("todo")

  // Exercise 10
  // Relative Difficulty: 10
  // Correctness: 5.0 marks
  // Performance: 2.5 marks
  // Elegance: 2.5 marks
  // Total: 10
  def reverse[A](as: List[A]): List[A] = error("todo")
}