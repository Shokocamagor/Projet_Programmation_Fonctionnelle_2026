package actors

import akka.actor.typed._
import akka.actor.typed.scaladsl._
import protocol._
import domain.Invariants
import simulation.SimulationLogger

import java.util.UUID

/**
 * QueenActor — L'acteur central de la colonie.
 * * RÔLES :
 * 1. Gestion du cycle de vie : Pond des œufs et gère leur progression.
 * 2. Régulation de la population : Assure que le nombre de fourmis ne dépasse pas MaxPerType.
 * 3. Survie : Doit être nourrie par les transporteuses pour ne pas mourir de faim.
 * 4. Monitoring : Vérifie les invariants du domaine à chaque cycle.
 */
object QueenActor {

  val MaxHunger:    Int = Invariants.MaxHunger
  val EggsPerCycle: Int = 2
  val MaxPerType:   Int = Invariants.MaxPerType

  /**
   * @param storage       Référence vers l'acteur de stockage.
   * @param hunger        Niveau de faim actuel (augmente avec le temps).
   * @param foragerCount  Nombre actuel de fourrageuses vivantes.
   * @param carrierCount  Nombre actuel de transporteuses vivantes.
   * @param foragerNextId Compteur pour attribuer un ID unique aux nouvelles fourrageuses.
   * @param carrierNextId Compteur pour attribuer un ID unique aux nouvelles transporteuses.
   * @param eggs          Liste des ActorRef des œufs actuellement en incubation.
   */
  def apply(
             storage:      ActorRef[Command],
             hunger:       Int = 5,
             foragerCount: Int = 1,   // on démarre avec 1 de chaque déjà en vie
             carrierCount: Int = 1 ,
             foragerNextId: Int = 2, // démarre à 2 — l'ID 1 est spawné dans Simulation
             carrierNextId: Int = 2,
             eggs:         List[ActorRef[Command]] = List.empty
  ): Behavior[Command] =
    Behaviors.receive { (context, message) =>
      message match {

        // ── Tick : la faim augmente, les œufs progressent ────────────────────
        case Tick =>
          val next = hunger + 1 // la faim augmente de 1 à chaque cycle
          SimulationLogger.logQueenTick(hunger, next)
          context.log.info(s"[Reine] Cycle suivant — état de faim : $hunger → $next")

          // Vérification des règles métier (Invariants)
          val violations = Invariants.checkQueen(next)
          if (violations.nonEmpty) {
            violations.foreach(v => context.log.warn(s"[Invariant] $v"))
            SimulationLogger.logInvariantViolation(violations)
          }
          if (next > MaxHunger) {
            context.log.error("[Reine] Faim trop élevée — la reine est morte. La colonie s'effondre.")
            SimulationLogger.logQueenDeath()
            Behaviors.stopped
          } else {
            // on notifie les œufs du passage du temps pour qu'ils progressent dans leur développement
            eggs.foreach(_ ! Tick)
            // Pond 2 nouveaux œufs
            val newEggs = (1 to EggsPerCycle).map { i =>
              val egg = context.spawn(
                EggActor(context.self, storage, context.self),
                s"egg-${UUID.randomUUID()}-$i"
              )
              context.log.info(s"[Reine] Œuf pondu.")
              egg
            }.toList

            // Mise à jour de l'état de la reine (Hunger + nouveaux œufs)
            QueenActor(storage, next, foragerCount, carrierCount, foragerNextId, carrierNextId,  eggs ++ newEggs)
          }

        // ── Nourrissage ──────────────────────────────────────────────────────
        case FeedQueen(amount) =>
          val next = (hunger - amount) max 0
          SimulationLogger.logQueenFed(amount, hunger, next)
          context.log.info(s"[Reine] Se Nourrie de $amount unité(s) — faim diminue : $hunger → $next")
          if (next == 0) context.log.info("[Reine] Totalement rassasiée.")
          QueenActor(storage, next, foragerCount, carrierCount,  foragerNextId, carrierNextId, eggs)

        // ── Éclosion d'œuf → naissance d'une fourmi ─────────────────────────
        case SpawnAnt(antType) =>
          antType match {
            case Forager =>
              val popViolations = Invariants.checkPopulation(foragerCount, carrierCount)
              if (popViolations.nonEmpty) SimulationLogger.logInvariantViolation(popViolations)

              if (foragerCount >= MaxPerType) {
                context.log.warn(s"[Reine] Trop de fourrageuses ($foragerCount/$MaxPerType) — nouvelle fourmi éliminée.")
                QueenActor(storage, hunger, foragerCount, carrierCount, foragerNextId, carrierNextId, eggs)
              } else {
                val id = foragerNextId
                val name = s"forager-$id"
                context.spawn(ColonyGuardianActor.supervisedForager(context.self, storage, id), name)
                SimulationLogger.logAntSpawned("Fourrageuse", id, foragerCount+1)
                context.log.info(s"[Reine] Nouvelle fourrageuse née ! Total : ${foragerCount + 1}/$MaxPerType")
                QueenActor(storage, hunger, foragerCount + 1, carrierCount, foragerNextId + 1, carrierNextId, eggs)
              }

            case Carrier =>
              val popViolations = Invariants.checkPopulation(foragerCount, carrierCount)
              if (popViolations.nonEmpty) SimulationLogger.logInvariantViolation(popViolations)

              if (carrierCount >= MaxPerType) {
                context.log.warn(s"[Reine] Trop de transporteuses ($carrierCount/$MaxPerType) — nouvelle fourmi éliminée.")
                QueenActor(storage, hunger, foragerCount, carrierCount, foragerNextId, carrierNextId, eggs)
              } else {
                val id = carrierNextId
                val name = s"carrier-$id"
                context.spawn(ColonyGuardianActor.supervisedCarrier(context.self, storage, id), name)
                SimulationLogger.logAntSpawned("Transporteuse", id, carrierCount+1)
                context.log.info(s"[Reine] Nouvelle transporteuse née ! Total : ${carrierCount + 1}/$MaxPerType")
                QueenActor(storage, hunger, foragerCount, carrierCount + 1, foragerNextId, carrierNextId + 1, eggs)
              }
          }

        // ── Mort d'une fourmi ────────────────────────────────────────────────
        case AntDied(antType, id) =>
          antType match {
            case Forager =>
              val total = (foragerCount - 1) max 0
              SimulationLogger.logAntDied("Fourrageuse", id, total)
              context.log.info(s"[Reine] Une fourrageuse est morte (id : $id). Total : $total/$MaxPerType")
              val violations = Invariants.checkPopulation(foragerCount-1, carrierCount)
              if (violations.nonEmpty) SimulationLogger.logInvariantViolation(violations)
              QueenActor(storage, hunger, (foragerCount - 1) max 0, carrierCount, foragerNextId, carrierNextId, eggs)
            case Carrier =>
              val total = (carrierCount - 1) max 0
              SimulationLogger.logAntDied("Transporteuse", id, total)
              context.log.info(s"[Reine] Une transporteuse est morte. Total : $total/$MaxPerType")
              val violations = Invariants.checkPopulation(foragerCount, carrierCount-1)
              if (violations.nonEmpty) SimulationLogger.logInvariantViolation(violations)
              QueenActor(storage, hunger, foragerCount, (carrierCount - 1) max 0, foragerNextId, carrierNextId ,eggs)
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