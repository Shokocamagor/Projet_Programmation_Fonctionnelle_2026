package actors

import akka.actor.typed._
import akka.actor.typed.scaladsl._
import protocol._
import domain.Invariants

object QueenActor {

  val MaxHunger:    Int = 10
  val EggsPerCycle: Int = 2
  val MaxPerType:   Int = 4

  def apply(
             storage:      ActorRef[Command],
             hunger:       Int = 5,
             foragerCount: Int = 1,   // on démarre avec 1 de chaque déjà en vie
             carrierCount: Int = 1 ,
             eggs:         List[ActorRef[Command]] = List.empty
  ): Behavior[Command] =
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
            // Relaye le Tick aux œufs en incubation
            eggs.foreach(_ ! Tick)
            // Pond 2 nouveaux œufs
            val newEggs = (1 to EggsPerCycle).map { i =>
              val egg = context.spawn(
                EggActor(context.self, storage, context.self),
                s"egg-${System.currentTimeMillis()}-$i"
              )
              context.log.info(s"[Reine] Œuf pondu.")
              egg
            }.toList
            QueenActor(storage, next, foragerCount, carrierCount, eggs ++ newEggs)
          }

        case FeedQueen(amount) =>
          val next = (hunger - amount) max 0
          context.log.info(s"[Reine] Se Nourrie de $amount unité(s) — faim diminue : $hunger → $next")
          if (next == 0) context.log.info("[Reine] Totalement rassasiée.")
          QueenActor(storage, next, foragerCount, carrierCount, eggs)

        case SpawnAnt(antType) =>
          antType match {
            case Forager =>
              if (foragerCount >= MaxPerType) {
                context.log.warn(s"[Reine] Trop de fourrageuses ($foragerCount/$MaxPerType) — nouvelle fourmi éliminée.")
                QueenActor(storage, hunger, foragerCount, carrierCount)
              } else {
                val name = s"forager-${System.currentTimeMillis()}"
                context.spawn(ForagerAntActor(context.self, storage), name)
                context.log.info(s"[Reine] Nouvelle fourrageuse née ! Total : ${foragerCount + 1}/$MaxPerType")
                QueenActor(storage, hunger, foragerCount + 1, carrierCount)
              }
            case Carrier =>
              if (carrierCount >= MaxPerType) {
                context.log.warn(s"[Reine] Trop de transporteuses ($carrierCount/$MaxPerType) — nouvelle fourmi éliminée.")
                QueenActor(storage, hunger, foragerCount, carrierCount)
              } else {
                val name = s"carrier-${System.currentTimeMillis()}"
                context.spawn(CarrierAntActor(context.self, storage), name)
                context.log.info(s"[Reine] Nouvelle transporteuse née ! Total : ${carrierCount + 1}/$MaxPerType")
                QueenActor(storage, hunger, foragerCount, carrierCount + 1)
              }
          }

        case AntDied(antType) =>
          antType match {
            case Forager =>
              context.log.info(s"[Reine] Une fourrageuse est morte. Total : ${foragerCount - 1}/$MaxPerType")
              QueenActor(storage, hunger, (foragerCount - 1) max 0, carrierCount,eggs)
            case Carrier =>
              context.log.info(s"[Reine] Une transporteuse est morte. Total : ${carrierCount - 1}/$MaxPerType")
              QueenActor(storage, hunger, foragerCount, (carrierCount - 1) max 0, eggs)
          }

        case Stop =>
          context.log.info("[Reine] Signal d'arrêt reçu. Fermeture.")
          Behaviors.stopped

        case _ =>
          context.log.warn(s"[Reine] Message inconnu : $message")
          Behaviors.same
      }
    }
}