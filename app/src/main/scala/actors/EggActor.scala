package actors

import akka.actor.typed._
import akka.actor.typed.scaladsl._
import protocol._
import simulation.SimulationLogger

import scala.util.Random

/**
 * EggActor — Gère le cycle de développement d'un futur membre de la colonie.
 * * RÔLE :
 * Simuler la période d'incubation. L'œuf ne fait rien d'autre qu'attendre
 * que le temps passe (via le message Tick) avant de se transformer en fourmi.
 * * CYCLE DE VIE :
 * 1. Création par la Reine.
 * 2. Incubation (pendant un nombre défini de cycles).
 * 3. Éclosion (choix aléatoire du métier).
 * 4. Arrêt (Behaviors.stopped).
 */
object EggActor {

  def apply(
             queen:   ActorRef[Command],
             storage: ActorRef[Command],
             colony:  ActorRef[Command]   // pour notifier la reine à l'éclosion
           ): Behavior[Command] =
    incubating(queen, storage, colony, cyclesLeft = 2)

  /**
   * État INCUBATING : L'œuf attend les signaux de temps (Tick).
   * * @param cyclesLeft Nombre de Ticks restants avant l'éclosion.
   */
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
            Behaviors.stopped // L'œuf disparaît du système car il est devenu une fourmi
          } else {
            incubating(queen, storage, colony, next)
          }
        case _ => Behaviors.same // L'œuf ignore les autres messages (comme SearchFood ou FeedQueen)
      }
    }
}