package actors

import akka.actor.typed._
import akka.actor.typed.scaladsl._
import domain.Invariants
import protocol._
import simulation.SimulationLogger

/**
 * StorageActor — Gestionnaire des ressources de la colonie.
 * * RÔLE :
 * Centraliser la nourriture, limiter la capacité maximale et répondre aux requêtes
 * de prélèvement ou de dépôt.
 * * CONCURRENCE :
 * Grâce au modèle d'acteur, les dépôts et retraits sont sérialisés, ce qui évite
 * tout problème de "race condition" (conflit d'accès) sur la variable du stock.
 */
object StorageActor {

  // Capacité maximale définie dans les invariants du domaine
  val MaxCapacity: Int = Invariants.MaxCapacity

  /**
   * @param stock Quantité actuelle de nourriture en réserve.
   */
  def apply(stock: Int = 0): Behavior[Command] =
    Behaviors.receive { (context, message) =>
      message match {

        // Dépôt de nourriture envoyé par les fourrageuses: on ajoute au stock, en respectant la capacité maximale
        case DepositFood(amount) =>
          val next = (stock + amount) min MaxCapacity
          SimulationLogger.logStorageDeposit(amount, stock, next)
          context.log.info(s"[Stockage] Dépôt de $amount unité(s). Stock : $stock → $next")
          val violations = Invariants.checkStorage(next)
          if (violations.nonEmpty) {
            violations.foreach(v => context.log.warn(s"[Invariant] $v"))
            SimulationLogger.logInvariantViolation(violations)
          }
          StorageActor(next)

        // Requête de nourriture envoyée par les transporteuses: on fournit jusqu'à "max" unités, ou moins si le stock est insuffisant
        case RequestFood(max, replyTo) =>
          if (stock > 0) {
            val given = stock min max
            SimulationLogger.logStorageProvide(given, stock, stock - given, replyTo.path.name)
            context.log.info(s"[Stockage] Fourniture de $given unité(s). Stock : $stock → ${stock - given}")
            // On répond à l'expéditeur — mais avec Akka typed on a besoin du replyTo
            // On gère ça via FoodReady envoyé dans le message, voir CarrierAntActor
            replyTo ! FoodReady(given)
            StorageActor(stock - given)
          } else {
            SimulationLogger.logStorageEmpty(replyTo.path.name)
            context.log.warn(s"[Stockage] Stock vide — rien à fournir.")
            replyTo ! StorageEmpty
            StorageActor(stock)
          }

        // Requête de consommation envoyée par les fourmis au repos: on fournit 1 unité si disponible, sinon on répond que le stock est vide
        case ConsumeFood(replyTo) =>
          if (stock > 0) {
            SimulationLogger.logStorageProvide(1, stock, stock - 1, replyTo.path.name)
            context.log.info(s"[Stockage] Fourmi nourrie (repos). Stock : $stock → ${stock - 1}")
            replyTo ! FoodReady(1)
            StorageActor(stock - 1)
          } else {
            SimulationLogger.logStorageEmpty(replyTo.path.name)
            context.log.warn(s"[Stockage] Stock vide — fourmi ne peut pas manger.")
            replyTo ! StorageEmpty
            StorageActor(stock)
          }
        case _ => Behaviors.same
      }
    }
}