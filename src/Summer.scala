object Summer {

  def main(args : Array[String]) {
    args.foreach(arg => println (arg + ": "  + Calculator.calculate(arg)))
  }
  
}