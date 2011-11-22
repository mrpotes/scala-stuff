object ProjectEuler {
  def main(args : Array[String]) : Unit = {
    problem1(1000, 3, 5)
    problem2(4000000)
    problem3(600851475143L)
    problem4(3)
  }

  /*
   * If we list all the natural numbers below 10 that are multiples of 3 or 5, 
   * we get 3, 5, 6 and 9. The sum of these multiples is 23.
   * 
   * Find the sum of all the multiples of 3 or 5 below 1000.
   */
  def problem1(args : Int*) : Unit = {
    val split = args.splitAt(1)
    val tester = (i : Int) => split._2.exists(i % _ == 0)
    val naturalNumbers = for(i <- 1 until split._1(0) if (tester(i))) yield i
    println("Problem 1: " + naturalNumbers.sum)
  }
  
  /*
   * By considering the terms in the Fibonacci sequence whose values do not exceed 
   * four million, find the sum of the even-valued terms.
   */
  def problem2(max : Int) : Unit = {
    def fib(i : Int, j: Int) : List[Int] = {
      if (i + j > max) return List(i, j)
      else return i :: fib (j, i+j)
    }
    
    println("Problem 2: "+fib(1,2).filter(_ % 2 == 0).sum)
  }
  
  /*
   * The prime factors of 13195 are 5, 7, 13 and 29.
   * 
   * What is the largest prime factor of the number 600851475143 ?
   */
  def problem3(composite: Long) : Unit = {
    def divisors(candidate: Long, filter: Long => Boolean = (x) => true) = {
      val maxDivisor = Math.round(Math.sqrt(candidate.toDouble))
      for (i <- 2L until maxDivisor if candidate % i == 0 && filter(i)) yield i
    }
    
    val primes = divisors(composite, divisors(_).isEmpty)
    println("Problem 3: "+primes.mkString(","))
  }
  
  /*
   * A palindromic number reads the same both ways. The largest palindrome made from 
   * the product of two 2-digit numbers is 9009 = 91 99.
   * 
   * Find the largest palindrome made from the product of two 3-digit numbers.
   */
  def problem4(numDigits : Int) : Unit = {
    val max = (Math.pow(10, numDigits) - 1).intValue()
    val min = Math.pow(10, numDigits - 1).intValue()
    def find(i : Int) : List[Int] = {
      if (i.equals(min - 1)) return List[Int]()
      for (j <- max to min by -1) {
        val candidate = (i*j).toString();
        if (candidate.equals(candidate.toCharArray().reverse.mkString(""))) return i*j :: find(i - 1)
      }
      return find(i - 1)
    }
    
    println("Problem 4: " + find(max).sortWith(_ < _).takeRight(1)(0))
  }
}
