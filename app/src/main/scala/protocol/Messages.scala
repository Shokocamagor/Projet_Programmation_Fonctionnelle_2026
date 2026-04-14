package protocol
import akka.actor.typed.ActorRef


// ── Types de fourmis ──────────────────────────────────────────────────────────
sealed trait AntType
case object Forager extends AntType
case object Carrier extends AntType

// ── Commandes (messages) du système ──────────────────────────────────────────
sealed trait Command

/** Messages pour la reine **/
case object Tick extends Command // Horloge globale — envoyée par Simulation à la Reine
case class FeedQueen(amount: Int) extends Command // Transporteuse → reine : livraison
case object Stop extends Command // Signal d'arrêt propre


/** Messages pour la fourmi (général) **/
case object SearchFood extends Command // Fourmi cherche à prendre de la nourriture dans le stockage
case class ConsumeFood(replyTo: ActorRef[Command]) extends Command // Fourmi → stockage : consomme 1 unité pour se nourrir au repos
case object AntTick extends Command // Horloge interne des fourmis — fatigue / famine


/** Messages pour le stockage **/
case class  DepositFood(amount: Int)   extends Command // Fourrageuse → stockage : dépôt de nourriture collectée
case class RequestFood(max: Int, replyTo: ActorRef[Command]) extends Command // Transporteuse → stockage : demande de nourriture pour la reine
case class  FoodReady(amount: Int) extends Command // Stockage → fourmi : nourriture disponible
case object StorageEmpty extends Command // Stockage → fourmi : stock vide

/** Autres **/
case class  AntDied(antType: AntType, id : Int) extends Command // Fourmi → reine : notification de mort (avec son ID pour la traçabilité)
case class SpawnAnt(antType: AntType) extends Command // Œuf → reine : demande de naissance d'une fourmi

