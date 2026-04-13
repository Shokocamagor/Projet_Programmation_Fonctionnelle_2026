package simulation

import java.io.{BufferedWriter, FileWriter, PrintWriter}
import java.time.Instant
import java.nio.file.{Files, Paths}

/**
 * Logger de simulation — écrit chaque événement en JSON Lines (`.jsonl`).
 *
 * Format d'une ligne :
 * {"ts":"2026-04-13T10:00:00.123Z","actor":"Reine","event":"FeedQueen","detail":"amount=3","hunger":4}
 *
 * Le fichier est vidé à chaque démarrage de simulation (truncate).
 * L'écriture est thread-safe via `synchronized`.
 */
object SimulationLogger {

  private val FilePath = "simulation_log.jsonl"
  private var writer: Option[PrintWriter] = None

  /** Ouvre (et vide) le fichier de log. À appeler une seule fois au démarrage. */
  def init(): Unit = synchronized {
    // Vide le fichier si il existe déjà
    Files.deleteIfExists(Paths.get(FilePath))
    val pw = new PrintWriter(new BufferedWriter(new FileWriter(FilePath, false)))
    writer = Some(pw)
    log("System", "SimulationStart", "Démarrage de la simulation", Map.empty)
  }

  /** Ferme proprement le fichier. À appeler à l'arrêt de la simulation. */
  def close(): Unit = synchronized {
    log("System", "SimulationStop", "Arrêt de la simulation", Map.empty)
    writer.foreach(_.close())
    writer = None
  }

  /**
   * Enregistre un événement.
   *
   * @param actor   Nom de l'acteur émetteur (ex: "Reine", "Stockage", "Fourrageuse-2")
   * @param event   Nom de l'événement (ex: "FeedQueen", "DepositFood", "AntDied")
   * @param detail  Description libre (ex: "amount=3")
   * @param state   État courant pertinent (ex: Map("hunger" -> "4", "stock" -> "12"))
   */
  def log(
    actor:  String,
    event:  String,
    detail: String,
    state:  Map[String, String]
  ): Unit = synchronized {
    writer.foreach { pw =>
      val ts      = Instant.now().toString
      val stateJson = state.map { case (k, v) => s""""$k":"$v"""" }.mkString(",")
      val statePart = if (stateJson.nonEmpty) s",$stateJson" else ""
      pw.println(
        s"""{"ts":"$ts","actor":"$actor","event":"$event","detail":"$detail"$statePart}"""
      )
      pw.flush()
    }
  }

  // ── Helpers sémantiques ──────────────────────────────────────────────────

  def logQueenTick(hunger: Int, next: Int): Unit =
    log("Reine", "Tick", s"faim $hunger → $next", Map("hunger" -> next.toString))

  def logQueenFed(amount: Int, hunger: Int, next: Int): Unit =
    log("Reine", "FeedQueen", s"reçoit $amount unité(s)", Map("hunger" -> next.toString, "delta" -> s"-$amount"))

  def logQueenDeath(): Unit =
    log("Reine", "QueenDied", "Faim maximale atteinte — colonie effondrée", Map.empty)

  def logAntSpawned(antType: String, id: Int, total: Int): Unit =
    log("Reine", "SpawnAnt", s"$antType-$id née", Map("type" -> antType, "id" -> id.toString, "total" -> total.toString))

  def logAntDied(antType: String, id: Int, total: Int): Unit =
    log("Reine", "AntDied", s"$antType-$id morte", Map("type" -> antType, "id" -> id.toString, "total" -> total.toString))

  def logEggHatched(antType: String): Unit =
    log("Œuf", "EggHatched", s"Éclosion → $antType", Map("type" -> antType))

  def logStorageDeposit(amount: Int, before: Int, after: Int): Unit =
    log("Stockage", "DepositFood", s"+$amount unité(s)", Map("before" -> before.toString, "after" -> after.toString))

  def logStorageProvide(amount: Int, before: Int, after: Int, to: String): Unit =
    log("Stockage", "ProvideFood", s"-$amount → $to", Map("before" -> before.toString, "after" -> after.toString))

  def logStorageEmpty(requester: String): Unit =
    log("Stockage", "StorageEmpty", s"stock vide pour $requester", Map("stock" -> "0"))

  def logForagerSearch(id: Int, found: Int): Unit =
    log(s"Fourrageuse-$id", "SearchFood", s"$found unité(s) trouvée(s)", Map("found" -> found.toString))

  def logForagerRest(id: Int, cycles: Int): Unit =
    log(s"Fourrageuse-$id", "Resting", s"repos — cycles restants : $cycles", Map("restCycles" -> cycles.toString))

  def logForagerStarving(id: Int, level: Int, max: Int): Unit =
    log(s"Fourrageuse-$id", "Starvation", s"famine $level/$max", Map("starvation" -> level.toString))

  def logCarrierDeliver(id: Int, amount: Int): Unit =
    log(s"Transporteuse-$id", "DeliverFood", s"livre $amount unité(s) à la reine", Map("amount" -> amount.toString))

  def logCarrierStarving(id: Int, level: Int, max: Int): Unit =
    log(s"Transporteuse-$id", "Starvation", s"famine $level/$max", Map("starvation" -> level.toString))

  def logInvariantViolation(violations: List[String]): Unit =
    violations.foreach { v =>
      log("Invariants", "Violation", v, Map.empty)
    }
}