package simulation

import akka.actor.typed._
import akka.actor.typed.scaladsl._
import actors.{QueenActor, StorageActor, ForagerAntActor, CarrierAntActor}
import protocol._
import scala.concurrent.duration._

object Simulation {

  def apply(): Behavior[Nothing] =
    Behaviors.setup[Nothing] { context =>

      context.log.info("[Simulation] Démarrage — Cycle 3 : Fourrageuse + Stockage + Transporteuse + Reine")
      val storage = context.spawn(StorageActor(stock = 0),         "storage")

      val queen = context.spawn(QueenActor(storage, hunger = 5), "queen")
      val forager = context.spawn(ForagerAntActor(queen, storage),        "forager-1")
      val carrier = context.spawn(CarrierAntActor(queen, storage), "carrier-1")

      // La faim de la reine augmente toutes les 4 secondes
      context.system.scheduler.scheduleAtFixedRate(1.second, 4.seconds)(
        () => queen ! Tick
      )(context.executionContext)

      Behaviors.empty
    }
}