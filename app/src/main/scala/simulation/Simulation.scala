package simulation

import akka.actor.typed._
import akka.actor.typed.scaladsl._
import actors.{QueenActor, WorkerAntActor}
import protocol._
import scala.concurrent.duration._

object Simulation {

  def apply(): Behavior[Nothing] =
    Behaviors.setup[Nothing] { context =>

      //context.log.info("[Simulation] Démarrage — Cycle 2 : Reine + Fourmi ouvrière")

      // Création de la reine et d'une fourmi ouvrière
      val queen = context.spawn(QueenActor(hunger = 5), "queen")
      val ant   = context.spawn(WorkerAntActor(queen), "worker-ant-1")

      // La faim de la reine augmente avec le temps
      context.system.scheduler.scheduleAtFixedRate(1.second, 2.seconds)(
        () => queen ! Tick
      )(context.executionContext)

      // La fourmi part chercher à manger régulièrement
      context.system.scheduler.scheduleAtFixedRate(1.second, 3.seconds)(
        () => ant ! SearchFood
      )(context.executionContext)

      Behaviors.empty
    }
}