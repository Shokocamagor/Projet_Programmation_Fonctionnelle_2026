package protocol

sealed trait Command

// Messages pour la reine
case class FeedQueen(amount: Int) extends Command
case object Tick extends Command
case object Stop extends Command

// Messages pour la fourmi
case object SearchFood                        extends Command
case class  FoodFound(amount: Int)            extends Command
case class  DeliverFood(amount: Int)          extends Command
case class ConsumeFood(replyTo: akka.actor.typed.ActorRef[Command]) extends Command

case object AntTick extends Command  // horloge propre aux fourmis

// Stockage
case class  DepositFood(amount: Int)   extends Command
case class RequestFood(max: Int, replyTo: akka.actor.typed.ActorRef[Command]) extends Command
case class  FoodReady(amount: Int)     extends Command
case object StorageEmpty               extends Command

// Œufs et éclosion
case class  AntDied(antType: AntType)                             extends Command
case class SpawnAnt(antType: AntType) extends Command
sealed trait AntType
case object Forager extends AntType
case object Carrier extends AntType