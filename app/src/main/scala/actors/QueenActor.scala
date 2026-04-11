package actors

import akka.actor.typed._
import akka.actor.typed.scaladsl._
import protocol._
import domain.Invariants

object QueenActor {

  val MaxHunger: Int = 10

  def apply(hunger: Int = 5): Behavior[Command] =
    Behaviors.receive { (context, message) =>
      message match {

        case Tick =>
          val next = hunger + 1
          context.log.info(s"[Reine] Cycle suivant — état de faim : $hunger → $next")
          val violations = Invariants.check(next)


          if (violations.nonEmpty)
            violations.foreach(v => context.log.warn(s"[Invariant] $v"))

          if (next > MaxHunger) {
            context.log.error("[Reine] Faim trop élevée — la reine est morte. La colonie s'effondre.")
            Behaviors.stopped
          } else {
            QueenActor(next)
          }

        case FeedQueen =>
          val next = (hunger - 1) max 0
          context.log.info(s"[Reine] Se Nourrie — faim diminue : $hunger → $next")

          val violations = Invariants.check(next)
          if (violations.nonEmpty)
            violations.foreach(v => context.log.warn(s"[Invariant] $v"))

          if (next == 0)
            context.log.info("[Reine] Totalement rassasiée.")
          QueenActor(next)

        case Stop =>
          context.log.info("[Reine] Signal d'arrêt reçu. Fermeture du système.")
          Behaviors.stopped

        case _ =>
          context.log.warn(s"[Reine] Message inconnu reçu : $message")
          Behaviors.same
      }
    }
}