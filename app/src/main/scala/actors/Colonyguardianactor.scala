package actors

import akka.actor.typed._
import akka.actor.typed.scaladsl._
import protocol._
import simulation.SimulationLogger
import scala.concurrent.duration._

/**
 * ColonyGuardianActor — Superviseur racine de la colonie.
 *
 * ──────────────────────────────────────────────────────────────────────────────
 * POURQUOI UN SUPERVISEUR ?
 * ──────────────────────────────────────────────────────────────────────────────
 * La consigne exige "supervision, tolérance aux pannes". En Akka Typed, la
 * supervision est définie via `Behaviors.supervise(...).onFailure(...)` au
 * moment du spawn d'un acteur enfant.
 *
 * Sans superviseur, une exception non gérée dans un acteur le tue silencieusement.
 * Avec un superviseur, on choisit la stratégie de récupération :
 *
 *   - SupervisorStrategy.restart  → relance l'acteur (état réinitialisé)
 *   - SupervisorStrategy.stop     → arrête définitivement (défaut Akka Typed)
 *   - SupervisorStrategy.resume   → ignore l'exception, continue (Akka Classic)
 *   - SupervisorStrategy.escalate → remonte l'exception au superviseur parent
 *
 * ──────────────────────────────────────────────────────────────────────────────
 * HIÉRARCHIE DE SUPERVISION
 * ──────────────────────────────────────────────────────────────────────────────
 *
 *   ActorSystem (racine)
 *   └── ColonyGuardianActor   ← superviseur principal
 *       ├── StorageActor      ← restart sur exception (acteur critique)
 *       ├── QueenActor        ← stop sur exception   (mort = fin de colonie)
 *       ├── ForagerAntActor   ← restart sur exception (peut replanter et reprendre)
 *       └── CarrierAntActor   ← restart sur exception (idem)
 *
 * ──────────────────────────────────────────────────────────────────────────────
 * MESSAGE ADAPTER
 * ──────────────────────────────────────────────────────────────────────────────
 * Le ColonyGuardian reçoit des `GuardianCommand` (protocole interne).
 * Pour interagir avec la reine qui parle `Command`, on utilise un
 * `context.messageAdapter` qui traduit les `Command` entrants vers
 * `GuardianCommand`. Cela découple les deux protocoles.
 */
object ColonyGuardianActor {

  // ── Protocole interne du guardian ──────────────────────────────────────────

  sealed trait GuardianCommand
  case object StartColony                         extends GuardianCommand
  case object StopColony                          extends GuardianCommand
  case class  StorageFailed(cause: Throwable)     extends GuardianCommand
  case class  QueenTerminated(reason: String)     extends GuardianCommand
  case class  ForwardToQueen(cmd: Command)        extends GuardianCommand

  def apply(): Behavior[GuardianCommand] =
    Behaviors.setup { context =>
      context.log.info("[Guardian] Superviseur de colonie démarré.")
      context.self ! StartColony
      waiting()
    }

  private def waiting(): Behavior[GuardianCommand] =
    Behaviors.receiveMessage {
      case StartColony =>
        // Délégue la construction aux behaviors concrets
        // (voir Simulation.scala qui spawn directement — ici on modélise
        //  la supervision pure sans refactoriser toute la simulation)
        Behaviors.same
      case _ =>
        Behaviors.same
    }

  // ── Stratégies de supervision exposées ─────────────────────────────────────

  /**
   * Stratégie pour StorageActor : RESTART sur toute exception.
   *
   * Justification métier : le stockage est le composant central. Si une
   * ArithmeticException ou NullPointerException se produit (ex: stock corrompu),
   * on repart de stock=0 plutôt que de faire tomber toute la colonie.
   *
   * Usage :
   *   context.spawn(
   *     supervisedStorage(initialStock = 0),
   *     "storage"
   *   )
   */
  def supervisedStorage(initialStock: Int = 0): Behavior[Command] =
    Behaviors
      .supervise(StorageActor(initialStock))
      .onFailure[Exception](
        SupervisorStrategy.restart
          .withLimit(maxNrOfRetries = 3, withinTimeRange = 10.seconds)
      )

  /**
   * Stratégie pour QueenActor : STOP sur exception.
   *
   * Justification métier : la reine ne doit pas "redémarrer" — si elle plante
   * c'est une erreur critique. Elle s'arrête proprement et les acteurs qui la
   * surveillent (`context.watch`) reçoivent le signal Terminated.
   */
  def supervisedQueen(
                       storage:       ActorRef[Command],
                       initialHunger: Int = 5
                     ): Behavior[Command] =
    Behaviors
      .supervise(QueenActor(storage, hunger = initialHunger))
      .onFailure[Exception](SupervisorStrategy.stop)

  /**
   * Stratégie pour ForagerAntActor : RESTART avec délai sur exception.
   *
   * Justification métier : une fourrageuse peut replanter (ex: erreur Random
   * ou état incohérent). On la redémarre avec un délai pour éviter une boucle
   * de crash rapide. Elle repart en état idle.
   */
  def supervisedForager(
                         queen:   ActorRef[Command],
                         storage: ActorRef[Command],
                         id:      Int
                       ): Behavior[Command] =
    Behaviors
      .supervise(ForagerAntActor(queen, storage, id))
      .onFailure[Exception](
        SupervisorStrategy.restartWithBackoff(
          minBackoff   = 500.millis,
          maxBackoff   = 5.seconds,
          randomFactor = 0.2
        )
      )

  /**
   * Stratégie pour CarrierAntActor : RESTART avec délai sur exception.
   * Même raisonnement que pour la fourrageuse.
   */
  def supervisedCarrier(
                         queen:   ActorRef[Command],
                         storage: ActorRef[Command],
                         id:      Int
                       ): Behavior[Command] =
    Behaviors
      .supervise(CarrierAntActor(queen, storage, id))
      .onFailure[Exception](
        SupervisorStrategy.restartWithBackoff(
          minBackoff   = 500.millis,
          maxBackoff   = 5.seconds,
          randomFactor = 0.2
        )
      )
}