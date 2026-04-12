package actors

import akka.actor.typed._
import akka.actor.typed.scaladsl._
import protocol._
import akka.actor.typed.Terminated
import scala.concurrent.duration._

object CarrierAntActor {

  val CarryCapacity: Int = 2 // la quantité que la transporteuse peut transporter à la fois
  val RestDuration: Int  = 1 // nombre de cycles de repos après une livraison
  val MaxStarvation: Int = 3 // nombre de cycles sans livraison avant de mourir de faim

  def apply(queen: ActorRef[Command], storage: ActorRef[Command]): Behavior[Command] =
    Behaviors.setup { context =>
      context.watch(queen)
      context.log.info("[Transporteuse] Prête.")
      // Scheduler interne — la transporteuse s'envoie ses propres ordres
      context.system.scheduler.scheduleAtFixedRate(2.seconds, 2.seconds)( // la transporteuse tente une livraison toutes les 2 secondes
        () => context.self ! SearchFood
      )(context.executionContext)
      context.system.scheduler.scheduleAtFixedRate(3.seconds, 4.seconds)( // horloge de fatigue pour les fourmis — toutes les 4 secondes
        () => context.self ! AntTick
      )(context.executionContext)
      idle(queen, storage, starvation = 0)
    }

  private def idle(queen: ActorRef[Command], storage: ActorRef[Command], starvation: Int): Behavior[Command] =
    Behaviors.receive[Command] { (context, message) =>
      message match {

        case SearchFood =>
          context.log.info(s"[Transporteuse] Demande $CarryCapacity unité(s) au stockage.")
          storage ! RequestFood(CarryCapacity, context.self)
          waiting(queen, storage, starvation)

        case AntTick =>
          val nextStarvation = starvation + 1
          context.log.info(s"[Transporteuse] En attente — cycles sans nourriture : $nextStarvation/$MaxStarvation")
          if (nextStarvation >= MaxStarvation) {
            context.log.error("[Transporteuse] Morte de faim après trop de cycles sans livraison.")
            queen ! AntDied(Carrier) // Notifie la reine de la mort de cette transporteuse
            Behaviors.stopped
          } else {
            idle(queen, storage, nextStarvation)
          }

        case _ => Behaviors.same
      }
    }.receiveSignal {
      case (context, Terminated(_)) =>
        context.log.info("[Transporteuse] La reine est morte. Arrêt.")
        Behaviors.stopped
    }

  // Attend la réponse du stockage
  private def waiting(queen: ActorRef[Command], storage: ActorRef[Command], starvation: Int): Behavior[Command] =
    Behaviors.receive[Command] { (context, message) =>
      message match {

        case FoodReady(amount) =>
          context.log.info(s"[Transporteuse] Reçu $amount unité(s) — livraison à la reine.")
          queen ! FeedQueen(amount)
          resting(queen, storage, restCycles = RestDuration, starvation = 0)

        case StorageEmpty =>
          context.log.warn("[Transporteuse] Stockage vide — retour en attente.")
          idle(queen, storage, starvation)

        case _ => Behaviors.same
      }
    }.receiveSignal {
      case (context, Terminated(_)) =>
        context.log.info("[Transporteuse] La reine est morte. Arrêt.")
        Behaviors.stopped
    }

  private def resting(queen: ActorRef[Command], storage: ActorRef[Command], restCycles: Int, starvation: Int): Behavior[Command] =
    Behaviors.receive[Command] { (context, message) =>
      message match {

        case AntTick =>
          val next = restCycles - 1
          context.log.info(s"[Transporteuse] Au repos — demande nourriture au stockage.")
          storage ! ConsumeFood(context.self)
          restingWaiting(queen, storage, next, starvation)

        case _ => Behaviors.same
      }
    }.receiveSignal {
      case (context, Terminated(_)) =>
        context.log.info("[Transporteuse] La reine est morte. Arrêt.")
        Behaviors.stopped
    }

  private def restingWaiting(queen: ActorRef[Command], storage: ActorRef[Command], restCycles: Int, starvation: Int): Behavior[Command] =
    Behaviors.receive[Command] { (context, message) =>
      message match {

        case FoodReady(_) =>
          context.log.info("[Transporteuse] Nourrie pendant le repos.")
          if (restCycles <= 0) {
            context.log.info("[Transporteuse] Reposée et nourrie — retour en service.")
            idle(queen, storage, starvation = 0)
          } else {
            resting(queen, storage, restCycles, starvation = 0)
          }

        case StorageEmpty =>
          val nextStarvation = starvation + 1
          context.log.warn(s"[Transporteuse] Pas de nourriture au repos — famine : $nextStarvation/$MaxStarvation")
          if (nextStarvation >= MaxStarvation) {
            context.log.error("[Transporteuse] Morte de faim.")
            queen ! AntDied(Carrier) // Notifie la reine de la mort de cette transporteuse
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
        context.log.info("[Transporteuse] La reine est morte. Arrêt.")
        Behaviors.stopped
    }
}