package protocol

sealed trait Command

// Messages pour la reine
case object FeedQueen extends Command
case object Tick extends Command
case object Stop extends Command

// Messages pour la fourmi
case object SearchFood                        extends Command
case class  FoodFound(amount: Int)            extends Command
case class  DeliverFood(amount: Int)          extends Command