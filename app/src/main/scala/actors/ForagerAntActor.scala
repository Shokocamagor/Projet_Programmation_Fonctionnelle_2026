package actors

import akka.actor.typed._
import akka.actor.typed.scaladsl._
import protocol._
import scala.util.Random
import akka.actor.typed.Terminated
import scala.concurrent.duration._

object ForagerAntActor {

  val RestDuration: Int = 1 // cycles de repos avant de retravailler
  val MaxStarvation: Int = 3 // cycles à jeun avant de mourir

  def apply(queen: ActorRef[Command], storage: ActorRef[Command]): Behavior[Command] =
    Behaviors.setup { context =>
      context.watch(queen) // surveille la reine
      context.log.info("[Fourrageuse] Prête.")
      // Scheduler interne — la fourmi s'envoie ses propres ordres
      context.system.scheduler.scheduleAtFixedRate(1.second, 6.seconds)( // la fourrageuse part chercher toutes les 6 secondes
        () => context.self ! SearchFood
      )(context.executionContext)
      context.system.scheduler.scheduleAtFixedRate(3.seconds, 4.seconds)( // horloge de fatigue pour les fourmis — toutes les 4 secondes
        () => context.self ! AntTick
      )(context.executionContext)
      idle(queen, storage, starvation = 0)
    }

  // État : disponible, attend un ordre
  private def idle(queen: ActorRef[Command],storage: ActorRef[Command], starvation: Int): Behavior[Command] =
    Behaviors.receive[Command] { (context, message) =>
      message match {
        case SearchFood =>
          context.log.info("[Fourrageuse] Part chercher de la nourriture...")
          val found = Random.nextInt(3) + 1
          context.log.info(s"[Fourrageuse] $found unité(s) trouvée(s) — dépôt au stockage.")
          storage ! DepositFood(found)
          resting(queen, storage, restCycles = RestDuration, starvation = 0)

        case AntTick =>
          val nextStarvation = starvation + 1
          context.log.info(s"[Fourrageuse] En attente — cycles sans nourriture : $nextStarvation/$MaxStarvation")
          if (nextStarvation >= MaxStarvation) {
            context.log.error("[Fourrageuse] Morte de faim après trop de cycles sans activité.")
            queen ! AntDied(Forager) // Notifie la reine de la mort de cette fourrageuse
            Behaviors.stopped
          } else {
            idle(queen, storage, nextStarvation)
          }

        case _ => Behaviors.same
      }
    }.receiveSignal {
      case (context, Terminated(_)) =>
        context.log.info("[Fourrageuse] Signal d'arrêt reçu. Fermeture.")
        Behaviors.stopped
    }

  // État : au repos, attend RestDuration ticks avant de retravailler
  private def resting(queen: ActorRef[Command],storage: ActorRef[Command], restCycles: Int, starvation: Int): Behavior[Command] =
    Behaviors.receive[Command] { (context, message) =>
      message match {

        case AntTick =>
          val next = restCycles - 1
          context.log.info(s"[Fourrageuse] Au repos — demande nourriture au stockage.")
          storage ! ConsumeFood(context.self)
          restingWaiting(queen, storage, next, starvation)

        case _ => Behaviors.same
      }
    }.receiveSignal {
      case (context, Terminated(_)) =>
        context.log.info("[Fourrageuse] La reine est morte. Arrêt.")
        Behaviors.stopped
    }

  private def restingWaiting(queen: ActorRef[Command], storage: ActorRef[Command], restCycles: Int, starvation: Int): Behavior[Command] =
    Behaviors.receive[Command] { (context, message) =>
      message match {

        case FoodReady(_) =>
          context.log.info("[Fourrageuse] Nourrie pendant le repos.")
          if (restCycles <= 0) {
            context.log.info("[Fourrageuse] Reposée et nourrie — retour en service.")
            idle(queen, storage, starvation = 0)
          } else {
            resting(queen, storage, restCycles, starvation = 0)
          }

        case StorageEmpty =>
          val nextStarvation = starvation + 1
          context.log.warn(s"[Fourrageuse] Pas de nourriture au repos — famine : $nextStarvation/$MaxStarvation")
          if (nextStarvation >= MaxStarvation) {
            context.log.error("[Fourrageuse] Morte de faim.")
            queen ! AntDied(Forager) // Notifie la reine de la mort de cette fourrageuse
            Behaviors.stopped
          } else if (restCycles <= 0) {
            idle(queen, storage, nextStarvation)
          } else {
            resting(queen, storage, restCycles, nextStarvation)
          }

        case _ => Behaviors.same
      }
    }.receiveSignal {
      case (context, Terminated(_)) =>
        context.log.info("[Fourrageuse] La reine est morte. Arrêt.")
        Behaviors.stopped
    }
}