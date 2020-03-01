package mowItNow
import mowItNow.LawnMower
object MowItNow extends App {
  def lawnMowing(instructions : Array[String], verbose : Boolean = false): Unit ={
    // Control function of the class LawnMower.
    try{
      val ordersArray : Array[String]= instructions

      // We take the first line to define the size of the terrain.
      val limit = ordersArray(0).split(" ").map(_.toInt)
      assert(limit.forall(_ >= 0)) // Checks that there are not negative values in the coordinates.

      // Then, for every two lines we make a run of the lawn mower.
      for (i <- 1 until ordersArray.length if i%2 == 1){
        println(s"Mower number ${(i + 1)/2}")

        // We extract and verify data from the first line.
        val initialStand = ordersArray(i).toUpperCase.split(" ")
        val initialPosition = initialStand.take(2).map(_.toInt)
        assert(initialPosition.forall(_ >= 0))

        // Initialization of the lawn mower
        val myMower = new LawnMower(limit ,
          initialPosition,
          initialStand(2)
        )
        println(myMower)

        // For every character in line 2, we try to match it with one of the valid actions.
        var orders =   ordersArray(i+1).toUpperCase
        for (order <- orders){
          order match{
            case 'A' => myMower.move(verbose)
            case 'D' | 'G' => myMower.rotate(order , verbose)
            case _ => if (verbose) println(s"$order is an invalid order.")
          }
        }
        println(s"$myMower Thanks for using MowItNow!®")
      }
    }catch{
      // This catch should be able to handle most format errors in the orders file.
      case nfe: NumberFormatException => println("File structure not valid.")
      case ind: ArrayIndexOutOfBoundsException => println("Starting orientation not valid.")
      case asrt: AssertionError => println("Some numbers are negative or not valid")
      case ex: Exception => ex.printStackTrace()
    }
  }


  def getFile(): Array[String] = {
    // Function to read the instructions file.
    println("Please enter the filepath to the instructions")
    val filepath = scala.io.StdIn.readLine()
    val fileSource = scala.io.Source.fromFile(filepath)
    val fileArray = fileSource.getLines.toArray
    fileSource.close
    return fileArray
  }


  val test = getFile()
  lawnMowing(test)
}
