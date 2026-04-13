package actors

import akka.actor.typed._
import akka.actor.typed.scaladsl._
import protocol._
import akka.actor.typed.Terminated
import simulation.SimulationLogger

import scala.concurrent.duration._

object CarrierAntActor {

  val CarryCapacity: Int = 2 // la quantité que la transporteuse peut transporter à la fois
  val RestDuration: Int  = 1 // nombre de cycles de repos après une livraison
  val MaxStarvation: Int = 3 // nombre de cycles sans livraison avant de mourir de faim

  def apply(queen: ActorRef[Command], storage: ActorRef[Command], id: Int): Behavior[Command] =
    Behaviors.setup { context =>
      context.watch(queen)
      context.log.info(s"[Transporteuse-$id] Prête.")
      // Scheduler interne — la transporteuse s'envoie ses propres ordres
      context.system.scheduler.scheduleAtFixedRate(2.seconds, 2.seconds)( // la transporteuse tente une livraison toutes les 2 secondes
        () => context.self ! SearchFood
      )(context.executionContext)
      context.system.scheduler.scheduleAtFixedRate(3.seconds, 4.seconds)( // horloge de fatigue pour les fourmis — toutes les 4 secondes
        () => context.self ! AntTick
      )(context.executionContext)
      idle(queen, storage, id, starvation = 0)
    }

  private def idle(queen: ActorRef[Command], storage: ActorRef[Command], id: Int, starvation: Int): Behavior[Command] =
    Behaviors.receive[Command] { (context, message) =>
      message match {
        case SearchFood =>
          context.log.info(s"[Transporteuse-$id] Demande $CarryCapacity unité(s) au stockage.")
          storage ! RequestFood(CarryCapacity, context.self)
          waiting(queen, storage, id, starvation)

        case AntTick =>
          val nextStarvation = starvation + 1
          SimulationLogger.logCarrierStarving(id, nextStarvation, MaxStarvation)
          context.log.info(s"[Transporteuse-$id] En attente — cycles sans nourriture : $nextStarvation/$MaxStarvation")
          if (nextStarvation >= MaxStarvation) {
            context.log.error(s"[Transporteuse-$id] Morte de faim après trop de cycles sans livraison.")
            queen ! AntDied(Carrier, id) // Notifie la reine de la mort de cette transporteuse
            Behaviors.stopped
          } else {
            idle(queen, storage, id, nextStarvation)
          }

        case _ => Behaviors.same
      }
    }.receiveSignal {
      case (context, Terminated(_)) =>
        context.log.info(s"[Transporteuse-$id] La reine est morte. Arrêt.")
        Behaviors.stopped
    }

  // Attend la réponse du stockage
  private def waiting(queen: ActorRef[Command], storage: ActorRef[Command], id: Int, starvation: Int): Behavior[Command] =
    Behaviors.receive[Command] { (context, message) =>
      message match {

        case FoodReady(amount) =>
          SimulationLogger.logCarrierDeliver(id, amount)
          context.log.info(s"[Transporteuse-$id] Reçu $amount unité(s) — livraison à la reine.")
          queen ! FeedQueen(amount)
          resting(queen, storage, id, restCycles = RestDuration, starvation = 0)

        case StorageEmpty =>
          context.log.warn(s"[Transporteuse-$id] Stockage vide — retour en attente.")
          idle(queen, storage, id, starvation)

        case _ => Behaviors.same
      }
    }.receiveSignal {
      case (context, Terminated(_)) =>
        context.log.info(s"[Transporteuse-$id] La reine est morte. Arrêt.")
        Behaviors.stopped
    }

  private def resting(queen: ActorRef[Command], storage: ActorRef[Command], id: Int, restCycles: Int, starvation: Int): Behavior[Command] =
    Behaviors.receive[Command] { (context, message) =>
      message match {
        case AntTick =>
          val next = restCycles - 1
          context.log.info(s"[Transporteuse-$id] Au repos — demande nourriture au stockage.")
          storage ! ConsumeFood(context.self)
          restingWaiting(queen, storage, id, next, starvation)

        case _ => Behaviors.same
      }
    }.receiveSignal {
      case (context, Terminated(_)) =>
        context.log.info(s"[Transporteuse-$id] La reine est morte. Arrêt.")
        Behaviors.stopped
    }

  private def restingWaiting(queen: ActorRef[Command], storage: ActorRef[Command], id: Int ,restCycles: Int, starvation: Int): Behavior[Command] =
    Behaviors.receive[Command] { (context, message) =>
      message match {
        case FoodReady(_) =>
          context.log.info(s"[Transporteuse-$id] Nourrie pendant le repos.")
          if (restCycles <= 0) {
            context.log.info(s"[Transporteuse-$id] Reposée et nourrie — retour en service.")
            idle(queen, storage, id, starvation = 0)
          } else {
            resting(queen, storage, id, restCycles, starvation = 0)
          }

        case StorageEmpty =>
          val nextStarvation = starvation + 1
          SimulationLogger.logCarrierStarving(id, nextStarvation, MaxStarvation)
          context.log.warn(s"[Transporteuse-$id] Pas de nourriture au repos — famine : $nextStarvation/$MaxStarvation")
          if (nextStarvation >= MaxStarvation) {
            context.log.error(s"[Transporteuse-$id] Morte de faim.")
            queen ! AntDied(Carrier, id) // Notifie la reine de la mort de cette transporteuse
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
        context.log.info(s"[Transporteuse-$id] La reine est morte. Arrêt.")
        Behaviors.stopped
    }
}