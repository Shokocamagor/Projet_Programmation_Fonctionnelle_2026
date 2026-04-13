package actors

import akka.actor.typed._
import akka.actor.typed.scaladsl._
import protocol._
import simulation.SimulationLogger

import scala.util.Random

object EggActor {

  def apply(
             queen:   ActorRef[Command],
             storage: ActorRef[Command],
             colony:  ActorRef[Command]   // pour notifier la reine à l'éclosion
           ): Behavior[Command] =
    incubating(queen, storage, colony, cyclesLeft = 2)

  private def incubating(
                          queen:      ActorRef[Command],
                          storage:    ActorRef[Command],
                          colony:     ActorRef[Command],
                          cyclesLeft: Int
                        ): Behavior[Command] =
    Behaviors.receive { (context, message) =>
      message match {
        case Tick =>
          val next = cyclesLeft - 1 // le temps d'incubation diminue à chaque tick
          context.log.info(s"[Œuf] Incubation — cycles restants : $next")
          if (next <= 0) { // l'œuf éclot ! On choisit aléatoirement le type de fourmi à créer
            val antType = if (Random.nextBoolean()) Forager else Carrier
            SimulationLogger.logEggHatched(antType.toString)
            context.log.info(s"[Œuf] Éclosion ! Type : $antType")
            colony ! SpawnAnt(antType)
            Behaviors.stopped
          } else {
            incubating(queen, storage, colony, next)
          }
        case _ => Behaviors.same
      }
    }
}