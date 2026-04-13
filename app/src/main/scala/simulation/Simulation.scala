package simulation

import akka.actor.typed._
import akka.actor.typed.scaladsl._
import actors.{QueenActor, StorageActor, ForagerAntActor, CarrierAntActor}
import protocol._
import scala.concurrent.duration._

object Simulation {

  def apply(): Behavior[Nothing] =
    Behaviors.setup[Nothing] { context =>

      // ── Initialise le logger (vide le fichier précédent) ──────────────────
      SimulationLogger.init()

      context.log.info("[Simulation] Démarrage — Colonie de fourmis")
      val storage = context.spawn(StorageActor(stock = 0), "storage")
      val queen = context.spawn(QueenActor(storage, hunger = 5), "queen")

      // Les fourmis initiales ont les IDs 1 — la reine commencera à 2 pour les suivantes
      val forager = context.spawn(ForagerAntActor(queen, storage, id = 1), "forager-1")
      val carrier = context.spawn(CarrierAntActor(queen, storage, id = 1), "carrier-1")

      SimulationLogger.log("Simulation", "Init",
        "Fourrageuse-1 et Transporteuse-1 spawned",
        Map("foragers" -> "1", "carriers" -> "1", "stock" -> "0", "hunger" -> "5")
      )

      // La faim de la reine augmente toutes les 4 secondes
      context.system.scheduler.scheduleAtFixedRate(1.second, 4.seconds)(
        () => queen ! Tick
      )(context.executionContext)

      Behaviors.empty
    }
}