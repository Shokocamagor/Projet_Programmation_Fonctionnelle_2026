package actors

import akka.actor.typed._
import akka.actor.typed.scaladsl._
import protocol._

import scala.util.Random
import akka.actor.typed.Terminated
import simulation.SimulationLogger

import scala.concurrent.duration._

object ForagerAntActor {

  val RestDuration: Int = 1 // cycles de repos avant de retravailler
  val MaxStarvation: Int = 3 // cycles à jeun avant de mourir

  def apply(queen: ActorRef[Command], storage: ActorRef[Command], id : Int): Behavior[Command] =
    Behaviors.setup { context =>
      context.watch(queen) // surveille la reine
      context.log.info(s"[Fourrageuse-$id] Prête.")
      // Scheduler interne — la fourmi s'envoie ses propres ordres
      context.system.scheduler.scheduleAtFixedRate(1.second, 6.seconds)( // la fourrageuse part chercher toutes les 6 secondes
        () => context.self ! SearchFood
      )(context.executionContext)
      context.system.scheduler.scheduleAtFixedRate(3.seconds, 4.seconds)( // horloge de fatigue pour les fourmis — toutes les 4 secondes
        () => context.self ! AntTick
      )(context.executionContext)
      idle(queen, storage, id, starvation = 0)
    }

  // État : disponible, attend un ordre
  private def idle(queen: ActorRef[Command],storage: ActorRef[Command], id : Int ,starvation: Int): Behavior[Command] =
    Behaviors.receive[Command] { (context, message) =>
      message match {
        case SearchFood =>
          val found = Random.nextInt(3) + 1
          SimulationLogger.logForagerSearch(id, found)
          context.log.info(s"[Fourrageuse-$id] Part chercher de la nourriture...")
          context.log.info(s"[Fourrageuse-$id] $found unité(s) trouvée(s) — dépôt au stockage.")
          storage ! DepositFood(found)
          resting(queen, storage, id, restCycles = RestDuration, starvation = 0)

        case AntTick =>
          val nextStarvation = starvation + 1
          SimulationLogger.logForagerStarving(id, nextStarvation, MaxStarvation)
          context.log.info(s"[Fourrageuse-$id] En attente — cycles sans nourriture : $nextStarvation/$MaxStarvation")
          if (nextStarvation >= MaxStarvation) {
            context.log.error(s"[Fourrageuse-$id] Morte de faim après trop de cycles sans activité.")
            queen ! AntDied(Forager, id) // Notifie la reine de la mort de cette fourrageuse
            Behaviors.stopped
          } else {
            idle(queen, storage, id, nextStarvation)
          }

        case _ => Behaviors.same
      }
    }.receiveSignal {
      case (context, Terminated(_)) =>
        context.log.info(s"[Fourrageuse-$id] Signal d'arrêt reçu. Fermeture.")
        Behaviors.stopped
    }

  // État : au repos, attend RestDuration ticks avant de retravailler
  private def resting(queen: ActorRef[Command],storage: ActorRef[Command], id: Int, restCycles: Int, starvation: Int): Behavior[Command] =
    Behaviors.receive[Command] { (context, message) =>
      message match {
        case AntTick =>
          val next = restCycles - 1
          SimulationLogger.logForagerRest(id, next)
          context.log.info(s"[Fourrageuse-$id] Au repos ($next cycles restants)  — demande nourriture au stockage.")
          storage ! ConsumeFood(context.self)
          restingWaiting(queen, storage, id, next, starvation)

        case _ => Behaviors.same
      }
    }.receiveSignal {
      case (context, Terminated(_)) =>
        context.log.info(s"[Fourrageuse-$id] La reine est morte. Arrêt.")
        Behaviors.stopped
    }


  // État : au repos, a demandé de la nourriture, attend la réponse du stockage
  private def restingWaiting(queen: ActorRef[Command], storage: ActorRef[Command], id: Int ,restCycles: Int, starvation: Int): Behavior[Command] =
    Behaviors.receive[Command] { (context, message) =>
      message match {

        case FoodReady(_) =>
          context.log.info(s"[Fourrageuse-$id] Nourrie pendant le repos.")
          if (restCycles <= 0) {
            context.log.info(s"[Fourrageuse-$id] Reposée et nourrie — retour en service.")
            idle(queen, storage, id, starvation = 0)
          } else {
            resting(queen, storage, restCycles, id, starvation = 0)
          }

        case StorageEmpty =>
          val nextStarvation = starvation + 1
          SimulationLogger.logForagerStarving(id, nextStarvation, MaxStarvation)
          context.log.warn(s"[Fourrageuse-$id] Pas de nourriture au repos — famine : $nextStarvation/$MaxStarvation")
          if (nextStarvation >= MaxStarvation) {
            context.log.error(s"[Fourrageuse-$id] Morte de faim.")
            queen ! AntDied(Forager,id) // Notifie la reine de la mort de cette fourrageuse
            Behaviors.stopped
          } else if (restCycles <= 0) {
            idle(queen, storage, id, nextStarvation)
          } else {
            resting(queen, storage, id, restCycles, nextStarvation)
          }

        case _ => Behaviors.same
      }
    }.receiveSignal {
      case (context, Terminated(_)) =>
        context.log.info(s"[Fourrageuse-$id] La reine est morte. Arrêt.")
        Behaviors.stopped
    }
}