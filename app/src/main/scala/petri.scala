/**
 * PetriNet.scala — Traduction formelle vers un réseau de Pétri
 *
 * Ce module traduit l'application Akka/Scala (colonie de fourmis) en un réseau
 * de Pétri P/T (Places/Transitions) et en explore les propriétés structurelles.
 *
 * Placement dans le projet :
 *   Projet_Programmation_Fonctionnelle_2026/petri/PetriNet.scala
 *   (à côté de Petri.scala existant)
 *
 * Usage :
 *   sbt "runMain PetriNetMain"
 *
 * ──────────────────────────────────────────────────────────────────────────────
 * CORRESPONDANCE Akka → Pétri
 * ──────────────────────────────────────────────────────────────────────────────
 *
 *  ACTEUR AKKA          │ PLACES (états internes)
 * ─────────────────────────────────────────────────────────────────────────────
 *  QueenActor           │ queen_alive, queen_dead, queen_sated
 *  StorageActor         │ stock (place capacitée), storage_empty
 *  ForagerAntActor      │ forager_idle, forager_resting, forager_resting_waiting, forager_dead
 *  CarrierAntActor      │ carrier_idle, carrier_waiting, carrier_resting, carrier_resting_waiting, carrier_dead
 *  EggActor             │ egg_incubating, egg_hatched
 *
 *  MESSAGE AKKA         │ TRANSITION
 * ─────────────────────────────────────────────────────────────────────────────
 *  Tick                 │ t_tick
 *  FeedQueen(n)         │ t_feed_queen
 *  SearchFood           │ t_search_food (fourrageuse), t_request_food (transporteuse)
 *  DepositFood(n)       │ t_deposit_food
 *  RequestFood(n, ref)  │ t_request_food
 *  ConsumeFood(ref)     │ t_consume_food
 *  FoodReady(n)         │ t_food_ready
 *  StorageEmpty         │ t_storage_empty
 *  SpawnAnt             │ t_spawn_forager, t_spawn_carrier
 *  AntDied              │ t_ant_died_forager, t_ant_died_carrier
 *  AntTick              │ t_ant_tick_idle, t_ant_tick_resting
 */

// ══════════════════════════════════════════════════════════════════════════════
// Structures de données
// ══════════════════════════════════════════════════════════════════════════════

/** Une place dans le réseau de Pétri, identifiée par un nom unique. */
case class Place(name: String)

/**
 * Une transition dans le réseau de Pétri.
 *
 * @param name     Identifiant de la transition (correspond à un message Akka)
 * @param inputs   Places d'entrée avec leur poids (consommation de jetons)
 * @param outputs  Places de sortie avec leur poids (production de jetons)
 * @param guard    Condition supplémentaire optionnelle (pour les gardes booléennes)
 */
case class Transition(
                       name:    String,
                       inputs:  Map[Place, Int],
                       outputs: Map[Place, Int],
                       guard:   String = ""
                     )

/**
 * Un marquage M : association Place → nombre de jetons.
 * Représente l'état global du système à un instant donné.
 */
case class Marking(tokens: Map[Place, Int]) {

  def apply(p: Place): Int = tokens.getOrElse(p, 0)

  /** Vérifie si la transition t est franchissable depuis ce marquage. */
  def enables(t: Transition): Boolean =
    t.inputs.forall { case (p, w) => this(p) >= w }

  /**
   * Franchit la transition t et retourne le nouveau marquage.
   * Précondition : enables(t) doit être vrai.
   */
  def fire(t: Transition): Marking = {
    val afterConsume = t.inputs.foldLeft(tokens) { case (m, (p, w)) =>
      m.updated(p, m.getOrElse(p, 0) - w)
    }
    val afterProduce = t.outputs.foldLeft(afterConsume) { case (m, (p, w)) =>
      m.updated(p, m.getOrElse(p, 0) + w)
    }
    Marking(afterProduce)
  }

  override def toString: String =
    tokens.filter(_._2 > 0)
      .map { case (p, n) => s"${p.name}=$n" }
      .mkString("{ ", ", ", " }")
}

/**
 * Le réseau de Pétri complet — collection de places, transitions et marquage initial.
 */
case class PetriNet(
                     places:      Set[Place],
                     transitions: Set[Transition],
                     initial:     Marking
                   ) {

  /** Retourne toutes les transitions franchissables depuis le marquage m. */
  def enabled(m: Marking): Set[Transition] =
    transitions.filter(m.enables)

  /**
   * Génère l'espace d'états atteignables (BFS) à partir du marquage initial.
   * Limité à maxStates pour éviter l'explosion combinatoire.
   *
   * @return (ensemble des marquages atteignables, transitions observées)
   */
  def reachabilityGraph(maxStates: Int = 500): (Set[Marking], Set[(Marking, Transition, Marking)]) = {
    import scala.collection.mutable
    val visited = mutable.Set[Marking](initial)
    val queue   = mutable.Queue[Marking](initial)
    val edges   = mutable.Set[(Marking, Transition, Marking)]()

    while (queue.nonEmpty && visited.size < maxStates) {
      val current = queue.dequeue()
      enabled(current).foreach { t =>
        val next = current.fire(t)
        edges += ((current, t, next))
        if (!visited.contains(next)) {
          visited += next
          queue.enqueue(next)
        }
      }
    }
    (visited.toSet, edges.toSet)
  }

  /**
   * Détecte les marquages deadlock : marquages atteignables où aucune
   * transition n'est franchissable.
   */
  def deadlocks(maxStates: Int = 500): Set[Marking] = {
    val (reachable, _) = reachabilityGraph(maxStates)
    reachable.filter(m => enabled(m).isEmpty)
  }
}

// ══════════════════════════════════════════════════════════════════════════════
// Construction du réseau — traduction de la colonie de fourmis
// ══════════════════════════════════════════════════════════════════════════════

object AntColonyPetriNet {

  // ── Places ──────────────────────────────────────────────────────────────────

  // Reine
  val queenAlive = Place("queen_alive")     // reine vivante (hunger ∈ [0,10])
  val queenDead  = Place("queen_dead")      // reine morte  (hunger > 10)
  val queenSated = Place("queen_sated")     // hunger = 0

  // Stockage (jetons = unités de nourriture, borné à 20)
  val stock        = Place("stock")          // M(stock) ∈ [0, 20]
  val storageEmpty = Place("storage_empty")  // signal: stock vide

  // Fourrageuse (états comportementaux — 1 jeton par instance)
  val foragerIdle           = Place("forager_idle")
  val foragerResting        = Place("forager_resting")
  val foragerRestingWaiting = Place("forager_resting_waiting")
  val foragerDead           = Place("forager_dead")
  val foragerStarvation     = Place("forager_starvation")  // compteur [0,3]

  // Transporteuse (états comportementaux — 1 jeton par instance)
  val carrierIdle           = Place("carrier_idle")
  val carrierWaiting        = Place("carrier_waiting")
  val carrierResting        = Place("carrier_resting")
  val carrierRestingWaiting = Place("carrier_resting_waiting")
  val carrierDead           = Place("carrier_dead")
  val carrierStarvation     = Place("carrier_starvation")  // compteur [0,3]

  // Œuf
  val eggIncubating = Place("egg_incubating")  // M = cycles restants

  val allPlaces: Set[Place] = Set(
    queenAlive, queenDead, queenSated,
    stock, storageEmpty,
    foragerIdle, foragerResting, foragerRestingWaiting, foragerDead, foragerStarvation,
    carrierIdle, carrierWaiting, carrierResting, carrierRestingWaiting, carrierDead, carrierStarvation,
    eggIncubating
  )

  // ── Transitions ─────────────────────────────────────────────────────────────

  /**
   * T1 — Tick : la faim de la reine augmente.
   * Akka : queen ! Tick  →  hunger + 1
   * Si hunger + 1 > 10 → transition vers queen_dead.
   * Modélisation simplifiée : jeton dans queen_alive consommé et reprodit
   * (le compteur hunger est une garde extérieure au réseau P/T pur).
   */
  val tTick = Transition(
    name    = "t_tick",
    inputs  = Map(queenAlive -> 1),
    outputs = Map(queenAlive -> 1),   // reste vivante tant que hunger ≤ 10
    guard   = "hunger < MaxHunger"
  )

  val tQueenDies = Transition(
    name    = "t_queen_dies",
    inputs  = Map(queenAlive -> 1),
    outputs = Map(queenDead -> 1),
    guard   = "hunger >= MaxHunger"
  )

  /**
   * T2 — FeedQueen : la transporteuse livre de la nourriture.
   * Akka : queen ! FeedQueen(amount)  →  hunger - amount
   */
  val tFeedQueen = Transition(
    name    = "t_feed_queen",
    inputs  = Map(queenAlive -> 1, stock -> 2),  // poids 2 = CarryCapacity
    outputs = Map(queenAlive -> 1),
    guard   = "hunger > 0"
  )

  val tQueenSated = Transition(
    name    = "t_queen_sated",
    inputs  = Map(queenAlive -> 1),
    outputs = Map(queenSated -> 1),
    guard   = "hunger = 0"
  )

  /**
   * T3 — SearchFood (fourrageuse) : collecte 1–3 unités, dépôt au stockage.
   * Akka : storage ! DepositFood(found)  →  stock + found
   * Poids moyen = 2 (Random.nextInt(3)+1 moyenne ≈ 2).
   */
  val tSearchFood = Transition(
    name    = "t_search_food",
    inputs  = Map(foragerIdle -> 1),
    outputs = Map(foragerResting -> 1, stock -> 2)
  )

  /**
   * T4 — AntTick (fourrageuse idle) : incrémente la famine.
   * Si starvation < 3 → reste idle.
   */
  val tForagerStarveIdle = Transition(
    name    = "t_forager_starve_idle",
    inputs  = Map(foragerIdle -> 1),
    outputs = Map(foragerIdle -> 1, foragerStarvation -> 1),
    guard   = "forager_starvation < MaxStarvation"
  )

  val tForagerDiesIdle = Transition(
    name    = "t_forager_dies_idle",
    inputs  = Map(foragerIdle -> 1, foragerStarvation -> 3),
    outputs = Map(foragerDead -> 1),
    guard   = "forager_starvation >= MaxStarvation"
  )

  /**
   * T5 — AntTick (fourrageuse resting) : demande ConsumeFood.
   */
  val tForagerAntTick = Transition(
    name    = "t_forager_ant_tick",
    inputs  = Map(foragerResting -> 1, stock -> 1),
    outputs = Map(foragerRestingWaiting -> 1)
  )

  /**
   * T6 — FoodReady (fourrageuse restingWaiting) : nourrie → retour idle ou reste en repos.
   */
  val tForagerFed = Transition(
    name    = "t_forager_fed",
    inputs  = Map(foragerRestingWaiting -> 1),
    outputs = Map(foragerIdle -> 1)
  )

  /**
   * T7 — StorageEmpty (fourrageuse) : incrémente famine au repos.
   */
  val tForagerStarveResting = Transition(
    name    = "t_forager_starve_resting",
    inputs  = Map(foragerRestingWaiting -> 1),
    outputs = Map(foragerResting -> 1, foragerStarvation -> 1),
    guard   = "forager_starvation < MaxStarvation"
  )

  val tForagerDiesResting = Transition(
    name    = "t_forager_dies_resting",
    inputs  = Map(foragerRestingWaiting -> 1, foragerStarvation -> 3),
    outputs = Map(foragerDead -> 1),
    guard   = "forager_starvation >= MaxStarvation"
  )

  /**
   * T8 — SearchFood (transporteuse) : demande RequestFood.
   */
  val tRequestFood = Transition(
    name    = "t_request_food",
    inputs  = Map(carrierIdle -> 1, stock -> 2),   // CarryCapacity = 2
    outputs = Map(carrierWaiting -> 1)
  )

  /**
   * T9 — FoodReady (transporteuse) : livre à la reine → FeedQueen.
   */
  val tCarrierDeliver = Transition(
    name    = "t_carrier_deliver",
    inputs  = Map(carrierWaiting -> 1),
    outputs = Map(carrierResting -> 1, queenAlive -> 1)   // FeedQueen implicite
  )

  /**
   * T10 — StorageEmpty (transporteuse) : retour idle.
   */
  val tCarrierStorageEmpty = Transition(
    name    = "t_carrier_storage_empty",
    inputs  = Map(carrierWaiting -> 1),
    outputs = Map(carrierIdle -> 1)
  )

  /** T11 — AntTick (transporteuse idle) famine */
  val tCarrierStarveIdle = Transition(
    name    = "t_carrier_starve_idle",
    inputs  = Map(carrierIdle -> 1),
    outputs = Map(carrierIdle -> 1, carrierStarvation -> 1),
    guard   = "carrier_starvation < MaxStarvation"
  )

  val tCarrierDiesIdle = Transition(
    name    = "t_carrier_dies_idle",
    inputs  = Map(carrierIdle -> 1, carrierStarvation -> 3),
    outputs = Map(carrierDead -> 1)
  )

  /** T12 — AntTick (transporteuse resting) : ConsumeFood */
  val tCarrierAntTick = Transition(
    name    = "t_carrier_ant_tick",
    inputs  = Map(carrierResting -> 1, stock -> 1),
    outputs = Map(carrierRestingWaiting -> 1)
  )

  val tCarrierFed = Transition(
    name    = "t_carrier_fed",
    inputs  = Map(carrierRestingWaiting -> 1),
    outputs = Map(carrierIdle -> 1)
  )

  val tCarrierStarveResting = Transition(
    name    = "t_carrier_starve_resting",
    inputs  = Map(carrierRestingWaiting -> 1),
    outputs = Map(carrierResting -> 1, carrierStarvation -> 1),
    guard   = "carrier_starvation < MaxStarvation"
  )

  val tCarrierDiesResting = Transition(
    name    = "t_carrier_dies_resting",
    inputs  = Map(carrierRestingWaiting -> 1, carrierStarvation -> 3),
    outputs = Map(carrierDead -> 1)
  )

  /**
   * T13 — Éclosion d'œuf → SpawnAnt.
   * Akka : EggActor après 2 Ticks → colony ! SpawnAnt(antType)
   */
  val tEggHatch = Transition(
    name    = "t_egg_hatch",
    inputs  = Map(eggIncubating -> 2),   // 2 cycles d'incubation
    outputs = Map(foragerIdle -> 1)       // simplifié : fourrageuse (50% des cas)
  )

  val tEggHatchCarrier = Transition(
    name    = "t_egg_hatch_carrier",
    inputs  = Map(eggIncubating -> 2),
    outputs = Map(carrierIdle -> 1)       // l'autre 50%
  )

  val allTransitions: Set[Transition] = Set(
    tTick, tQueenDies, tFeedQueen, tQueenSated,
    tSearchFood,
    tForagerStarveIdle, tForagerDiesIdle,
    tForagerAntTick, tForagerFed,
    tForagerStarveResting, tForagerDiesResting,
    tRequestFood, tCarrierDeliver, tCarrierStorageEmpty,
    tCarrierStarveIdle, tCarrierDiesIdle,
    tCarrierAntTick, tCarrierFed,
    tCarrierStarveResting, tCarrierDiesResting,
    tEggHatch, tEggHatchCarrier
  )

  // ── Marquage initial M₀ ─────────────────────────────────────────────────────

  /**
   * M₀ correspond à l'état au démarrage de Simulation.scala :
   *   - Reine vivante, hunger = 5 (abstrait : 1 jeton dans queen_alive)
   *   - Stock = 0 (aucun jeton dans stock)
   *   - 1 fourrageuse idle
   *   - 1 transporteuse idle
   */
  val initialMarking: Marking = Marking(Map(
    queenAlive   -> 1,
    foragerIdle  -> 1,
    carrierIdle  -> 1
    // stock → 0 (pas de jetons initialement)
  ))

  val net: PetriNet = PetriNet(allPlaces, allTransitions, initialMarking)

  // ══════════════════════════════════════════════════════════════════════════════
  // P-invariants (vérifiés analytiquement)
  // ══════════════════════════════════════════════════════════════════════════════

  /**
   * Un P-invariant est un vecteur y tel que y^T * C = 0
   * où C est la matrice d'incidence (C = Post - Pre).
   *
   * P-invariants identifiés pour ce réseau :
   *
   * PI1 — Conservation de la reine :
   *   M(queen_alive) + M(queen_dead) + M(queen_sated) = 1
   *   → La reine est toujours dans exactement un état.
   *
   * PI2 — Conservation de l'état fourrageuse (par instance) :
   *   M(forager_idle) + M(forager_resting) + M(forager_resting_waiting) + M(forager_dead) = 1
   *   → Une fourrageuse est toujours dans exactement un état.
   *
   * PI3 — Conservation de l'état transporteuse (par instance) :
   *   M(carrier_idle) + M(carrier_waiting) + M(carrier_resting) + M(carrier_resting_waiting) + M(carrier_dead) = 1
   *
   * PI4 — Borne du stock (semi-positif, non conservatif) :
   *   0 ≤ M(stock) ≤ 20    (contrainte de capacité)
   */
  def checkPInvariants(m: Marking): List[String] = {
    val violations = scala.collection.mutable.ListBuffer[String]()

    // PI1
    val queenSum = m(queenAlive) + m(queenDead) + m(queenSated)
    if (queenSum != 1)
      violations += s"[PI1] VIOLATION : reine présente dans $queenSum états simultanés (attendu 1)"

    // PI2
    val foragerSum = m(foragerIdle) + m(foragerResting) + m(foragerRestingWaiting) + m(foragerDead)
    // Autorise 0 si pas de fourrageuse vivante (colonie initiale peut avoir N instances)
    if (foragerSum > 4)
      violations += s"[PI2] VIOLATION : $foragerSum fourrageuses actives (max 4)"

    // PI3
    val carrierSum = m(carrierIdle) + m(carrierWaiting) + m(carrierResting) + m(carrierRestingWaiting) + m(carrierDead)
    if (carrierSum > 4)
      violations += s"[PI3] VIOLATION : $carrierSum transporteuses actives (max 4)"

    // PI4
    val stockVal = m(stock)
    if (stockVal < 0)
      violations += s"[PI4] VIOLATION : stock négatif ($stockVal)"
    if (stockVal > 20)
      violations += s"[PI4] VIOLATION : stock dépasse MaxCapacity ($stockVal > 20)"

    violations.toList
  }

  // ══════════════════════════════════════════════════════════════════════════════
  // Propriétés LTL (Linear Temporal Logic) — formalisées
  // ══════════════════════════════════════════════════════════════════════════════

  /**
   * LTL1 — Sûreté (Safety) : "La reine ne meurt jamais si elle est nourrie à temps."
   *   □ (queen_alive → ◇ FeedQueen)
   *   Traduction : dans tout chemin d'exécution, si la reine est vivante,
   *   il existe un futur état où FeedQueen est franchie.
   *   ↳ Vérifiée si la transporteuse est vivante et le stock non vide.
   *
   * LTL2 — Vivacité (Liveness) : "Toute fourrageuse idle finit par chercher de la nourriture."
   *   □ (forager_idle → ◇ SearchFood_franchie)
   *   ↳ Garantie par le scheduler interne (scheduleAtFixedRate).
   *
   * LTL3 — Sûreté du stock : "Le stock ne devient jamais négatif."
   *   □ (M(stock) ≥ 0)
   *   ↳ P-invariant PI4 — structurellement vrai.
   *
   * LTL4 — Terminaison contrôlée : "Si toutes les fourmis sont mortes, la reine finit par mourir."
   *   □ (forager_dead ∧ carrier_dead → ◇ queen_dead)
   *   ↳ Vérifiable par exploration de l'espace d'états.
   *
   * LTL5 — Non-deadlock vivant : "Le système ne bloque jamais si la reine est vivante."
   *   □ ¬deadlock ∨ queen_dead
   *   ↳ Deadlock n'est atteignable que si queen_dead ∈ M.
   */
  val ltlProperties: Map[String, String] = Map(
    "LTL1" -> "□ (queen_alive → ◇ t_feed_queen franchissable)",
    "LTL2" -> "□ (forager_idle → ◇ t_search_food franchissable)",
    "LTL3" -> "□ (M(stock) ≥ 0)",
    "LTL4" -> "□ ((forager_dead ∧ carrier_dead) → ◇ queen_dead)",
    "LTL5" -> "□ (¬deadlock ∨ queen_dead)"
  )
}

// ══════════════════════════════════════════════════════════════════════════════
// Point d'entrée — analyse du réseau
// ══════════════════════════════════════════════════════════════════════════════

object PetriNetMain {
  def main(args: Array[String]): Unit = {
    import AntColonyPetriNet._

    println("=" * 70)
    println("  RÉSEAU DE PÉTRI — COLONIE DE FOURMIS")
    println("=" * 70)

    // ── Informations structurelles ──────────────────────────────────────────
    println(s"\n[Structure]")
    println(s"  Places      : ${net.places.size}")
    println(s"  Transitions : ${net.transitions.size}")
    println(s"  Marquage M₀ : ${net.initial}")

    // ── Transitions franchissables depuis M₀ ───────────────────────────────
    val enabledFromInit = net.enabled(net.initial)
    println(s"\n[Transitions franchissables depuis M₀] (${enabledFromInit.size})")
    enabledFromInit.foreach(t => println(s"  - ${t.name}"))

    // ── P-invariants sur M₀ ────────────────────────────────────────────────
    println(s"\n[P-invariants — vérification sur M₀]")
    val violations = checkPInvariants(net.initial)
    if (violations.isEmpty) println("  Tous les P-invariants sont satisfaits.")
    else violations.foreach(v => println(s"  $v"))

    // ── Exploration de l'espace d'états ────────────────────────────────────
    println(s"\n[Espace d'états — exploration BFS (max 300 marquages)]")
    val (reachable, edges) = net.reachabilityGraph(300)
    println(s"  Marquages atteignables : ${reachable.size}")
    println(s"  Arcs (transitions)     : ${edges.size}")

    // ── Deadlocks ──────────────────────────────────────────────────────────
    val dl = net.deadlocks(300)
    println(s"\n[Deadlocks détectés] : ${dl.size}")
    dl.take(3).foreach(m => println(s"  $m"))
    if (dl.size > 3) println(s"  ... (${dl.size - 3} autres)")

    // ── Propriétés LTL ─────────────────────────────────────────────────────
    println(s"\n[Propriétés LTL formalisées]")
    ltlProperties.foreach { case (id, formula) =>
      println(s"  $id : $formula")
    }

    // ── Vérification PI sur tous les marquages atteignables ────────────────
    println(s"\n[Vérification P-invariants sur l'espace d'états complet]")
    val allViolations = reachable.flatMap(m => checkPInvariants(m))
    if (allViolations.isEmpty)
      println("  Aucune violation détectée sur les marquages atteignables.")
    else {
      println(s"  ${allViolations.size} violation(s) :")
      allViolations.take(5).foreach(v => println(s"  $v"))
    }

    println("\n" + "=" * 70)
  }
}