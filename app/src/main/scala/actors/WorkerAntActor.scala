package actors

import akka.actor.typed._
import akka.actor.typed.scaladsl._
import protocol._
import scala.util.Random
import akka.actor.typed.Terminated

object WorkerAntActor {

  def apply(queen: ActorRef[Command]): Behavior[Command] =
    Behaviors.setup { context =>
      context.watch(queen) // surveille la reine
      idle(queen)
    }

  // État : la fourmi est disponible, elle attend l'ordre de chercher
  private def idle(queen: ActorRef[Command]): Behavior[Command] =
    Behaviors.receive[Command] { (context, message) =>
      message match {
        case SearchFood =>
          context.log.info("[Fourmi] À la recherche de nourriture...")
          val found = Random.nextInt(3) + 1
          context.log.info(s"[Fourmi] $found unité(s) de nourriture trouvée(s) !")
          context.self ! FoodFound(found)
          searching(queen)
        case _ => Behaviors.same
      }
    }.receiveSignal {
      case (context, Terminated(_)) =>
        context.log.info("[Fourmi] La reine est morte. La fourmi s'arrête.")
        Behaviors.stopped
      }

  // État : la fourmi a trouvé de la nourriture, elle la ramène
  private def searching(queen: ActorRef[Command]): Behavior[Command] =
    Behaviors.receive[Command] { (context, message) =>
      message match {
        case FoodFound(amount) =>
          context.log.info(s"[Fourmi] Livraison de $amount unité(s) à la reine.")
          queen ! FeedQueen
          idle(queen)
        case _ => Behaviors.same
      }
    }.receiveSignal {
      case (context, Terminated(_)) =>
        context.log.info("[Fourmi] La reine est morte. La fourmi s'arrête.")
        Behaviors.stopped
    }
}