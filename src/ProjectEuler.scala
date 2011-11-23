object ProjectEuler {
  def main(args : Array[String]) : Unit = {
//    problem1(1000, 3, 5)
//    problem2(4000000)
//    problem3(600851475143L)
//    problem4(3)
    problem5(20)
//    problem6(100)
//    problem8()
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
  
  def problem5(maxDivisor : Int) : Unit = {
    val range = 2 to maxDivisor
    def findit(test : Int = maxDivisor) : Int = {
      if (range.forall(test % _ == 0)) return test
      else return findit(test + maxDivisor)
    }
    println("Problem 5: "+findit())
  }
  
  def problem6(maxNaturalNumber : Int) : Unit = {
    val range = 1 to maxNaturalNumber
    val sumOfSquares = range.map(Math.pow(_, 2)).sum
    val squareOfSum = Math.pow(range.sum, 2)
    println("Problem 6: "+Math.abs((sumOfSquares-squareOfSum)).intValue())
  }
  
  def problem8() : Unit = {
    val numberString = "7316717653133062491922511967442657474235534919493496983520312774506326239578318016984801869478851843858615607891129494954595017379583319528532088055111254069874715852386305071569329096329522744304355766896648950445244523161731856403098711121722383113622298934233803081353362766142828064444866452387493035890729629049156044077239071381051585930796086670172427121883998797908792274921901699720888093776657273330010533678812202354218097512545405947522435258490771167055601360483958644670632441572215539753697817977846174064955149290862569321978468622482839722413756570560574902614079729686524145351004748216637048440319989000889524345065854122758866688116427171479924442928230863465674813919123162824586178664583591245665294765456828489128831426076900422421902267105562632111110937054421750694165896040807198403850962455444362981230987879927244284909188845801561660979191338754992005240636899125607176060588611646710940507754100225698315520005593572972571636269561882670428252483600823257530420752963450"
    val asciiZero = "0".first
    val numbers = numberString.toCharArray().map(_.intValue() - asciiZero.intValue())
    println("Problem 8: "+numbers.sliding(5,1).map(_.foldLeft(1)(_ * _)).max)
  }
}
