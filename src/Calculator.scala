import scala.collection.mutable.Map

object Calculator {

  private val cache = Map[String,Int]()
  
  def calculate(s:String) : Int = {
    cache.getOrElse(s, {
      Console.print("Calculating...")
      val calc = new Calculator
      s.getBytes().foreach(calc.add(_))
      cache += (s -> calc.checksum())
      cache(s)
    })
  }
}

class Calculator {
  private var sum = 0
  
  def add(b:Byte) { sum += b }
  def checksum() : Int = ~(sum & 0xFF) + 1
}