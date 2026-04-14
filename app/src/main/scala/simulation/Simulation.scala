package simulation

import akka.actor.typed._
import akka.actor.typed.scaladsl._
import actors.{ColonyGuardianActor, QueenActor, StorageActor, ForagerAntActor, CarrierAntActor}
import protocol._
import scala.concurrent.duration._

/**
 * Simulation — point d'entrée du système de colonie de fourmis.
 *
 * ──────────────────────────────────────────────────────────────────────────────
 * SUPERVISION (tolérance aux pannes)
 * ──────────────────────────────────────────────────────────────────────────────
 * Chaque acteur est spawné via ColonyGuardianActor.supervised* qui enveloppe
 * le Behavior dans une stratégie Behaviors.supervise(...).onFailure(...).
 *
 *   StorageActor    → restart (max 3 fois / 10s) — acteur critique, doit survivre
 *   QueenActor      → stop                        — mort = fin de partie
 *   ForagerAntActor → restartWithBackoff           — peut se remettre d'une erreur
 *   CarrierAntActor → restartWithBackoff           — idem
 *
 * ──────────────────────────────────────────────────────────────────────────────
 * MESSAGE ADAPTER
 * ──────────────────────────────────────────────────────────────────────────────
 * SimulationMonitor parle le protocole MonitorCommand (interne).
 * context.messageAdapter traduit Command → MonitorCommand.
 * Cela découple le protocole externe (Command) du protocole interne du monitor.
 */
object Simulation {

  def apply(): Behavior[Nothing] =
    Behaviors.setup[Nothing] { context =>

      SimulationLogger.init()
      context.log.info("[Simulation] Démarrage — Colonie de fourmis (avec supervision)")

      // ── Spawn avec supervision ──────────────────────────────────────────────

      val storage = context.spawn(
        ColonyGuardianActor.supervisedStorage(initialStock = 0),
        "storage"
      )

      val queen = context.spawn(
        ColonyGuardianActor.supervisedQueen(storage, initialHunger = 5),
        "queen"
      )

      context.spawn(
        ColonyGuardianActor.supervisedForager(queen, storage, id = 1),
        "forager-1"
      )
      context.spawn(
        ColonyGuardianActor.supervisedCarrier(queen, storage, id = 1),
        "carrier-1"
      )

      SimulationLogger.log("Simulation", "Init",
        "Supervision activée — Fourrageuse-1 et Transporteuse-1 spawned",
        Map(
          "foragers"         -> "1",
          "carriers"         -> "1",
          "stock"            -> "0",
          "hunger"           -> "5",
          "storage_strategy" -> "restart_max3_10s",
          "ant_strategy"     -> "restartWithBackoff_500ms_5s"
        )
      )

      // ── Monitor : observe la reine via watch + messageAdapter ───────────────
      context.spawn(SimulationMonitor(queen), "simulation-monitor")

      // ── Tick toutes les 4 secondes ─────────────────────────────────────────
      context.system.scheduler.scheduleAtFixedRate(1.second, 4.seconds)(
        () => queen ! Tick
      )(context.executionContext)

      Behaviors.empty
    }
}

/**
 * SimulationMonitor — surveille la reine et illustre le pattern MessageAdapter.
 *
 * PATTERN MESSAGE ADAPTER :
 *   Un acteur A (type MonitorCommand) veut recevoir des messages de type Command
 *   (protocole de la reine) sans exposer son type interne.
 *
 *   context.messageAdapter[Command] { cmd => ... } crée un ActorRef[Command]
 *   qui traduit automatiquement chaque Command reçu vers un MonitorCommand.
 *
 *   Cas d'usage réel : API REST → acteur interne, réponses d'un service externe
 *   vers le protocole métier, etc.
 */
object SimulationMonitor {

  sealed trait MonitorCommand
  case class MonitorObserved(event: String) extends MonitorCommand
  case object MonitorColonyDied             extends MonitorCommand

  def apply(queen: ActorRef[Command]): Behavior[MonitorCommand] =
    Behaviors.setup { context =>

      // ── MessageAdapter : ActorRef[Command] → MonitorCommand ───────────────
      // Toute entité externe peut envoyer un Command à cet adapter,
      // le monitor le reçoit traduit en MonitorCommand.
      // Exemple : la reine pourrait notifier ce monitor de chaque FeedQueen
      // sans jamais connaître le type MonitorCommand.
      val _: ActorRef[Command] =
        context.messageAdapter[Command] {
          case FeedQueen(n)  => MonitorObserved(s"Reine nourrie de $n unité(s)")
          case AntDied(t, i) => MonitorObserved(s"$t-$i est mort(e)")
          case Tick          => MonitorObserved("Tick global reçu")
          case _             => MonitorObserved("Événement système")
        }

      context.watch(queen)
      context.log.info("[Monitor] Surveillance de la reine activée.")

      Behaviors.receiveMessage[MonitorCommand] {
        case MonitorObserved(event) =>
          context.log.info(s"[Monitor] $event")
          Behaviors.same

        case MonitorColonyDied =>
          context.log.warn("[Monitor] Colonie effondrée — arrêt du monitor.")
          SimulationLogger.log("Monitor", "ColonyCollapsed", "Fin détectée", Map.empty)
          Behaviors.stopped

      }.receiveSignal {
        case (ctx, Terminated(ref)) =>
          ctx.log.warn(s"[Monitor] ${ref.path.name} terminé — colonie s'effondre.")
          SimulationLogger.log(
            "Monitor", "ActorTerminated",
            s"Signal Terminated de ${ref.path.name}",
            Map("actor" -> ref.path.name)
          )
          Behaviors.stopped
      }
    }
}