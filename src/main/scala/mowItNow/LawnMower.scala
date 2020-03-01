package mowItNow

class LawnMower(limit:Array[Int], startPosition:Array[Int], startDirection:String) {
  // Class attributes
  private val cardinalPoints = List("E" , "N" , "W" , "S")
  private val cardinalNames = Array("East" , "North" , "West" , "South")

  // Object Attributes
  private val maxSize = limit
  private var position = startPosition
  private var orientation : Int = this.cardinalPoints.indexOf(startDirection)

  // Masking toSting to allow println to take in the lawn mower class in the desired way.
  override def toString: String = s"The lawn mower is currently at (${this.position.mkString(", ")}), facing ${cardinalNames(orientation)}."

  // Class Methods
  /* All methods use a "verbose" boolean to control how much information is provided of the inner workings of the lawn
  mower. verbose = false conforms with the assignment requirements.*/

  /*These methods allow us to take advantage of the circular nature of a compass.
  Movements to the left (G) are counterclockwise, thus positive, and the opposite is true for movements to the
  right (D). The modulo operator allows us to always end up within [0,3].*/

  def rotate(turn:Char , verbose: Boolean =false): Unit ={
    val newCardinal : Int = this.orientation + {if (turn == 'D') -1 else 1}
    this.orientation = (newCardinal + 4) % 4
    if (verbose) println(s"The lawn mower is now facing ${this.cardinalNames(this.orientation)}.")
  }

  /*We have East (index 0) and West (index 2) that affect our position in the primary axis, while North (index 1)
  and South (index 3) affect our position in the secondary axis. Again the modulo operator allows us to easily
  spot this behaviour. In regards to the advancement, steps taken towards the north and the east move us closer to
  the limits. This means that when the index is under 2, we approach the limits, and we move away from them by going
  to the south and the west (index above or 2).*/

  def move(verbose: Boolean =false): Unit ={
    val nextSquare = this.position
    val movement = {if (this.orientation < 2) 1 else -1}
    val axis = this.orientation % 2
    nextSquare(axis) += movement
    // Before assigning the new position, we verify that the position is valid.
    if (0 <= nextSquare(axis) & nextSquare(axis) <= this.maxSize(axis)){
      this.position = nextSquare
      if (verbose) println(s"The lawn mower has moved to the ${this.cardinalNames(this.orientation)}.")
    } else if (verbose) println("The lawn mower cannot move there.")
  }
}

