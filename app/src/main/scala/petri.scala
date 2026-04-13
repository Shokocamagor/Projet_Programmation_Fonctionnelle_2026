/**
 * PetriNet.scala — Réseau de Pétri P/T modélisant la colonie de fourmis Akka
 *
 * ══════════════════════════════════════════════════════════════════════════════
 * CORRESPONDANCE Akka → Pétri
 * ══════════════════════════════════════════════════════════════════════════════
 *
 *  ACTEUR AKKA           │ PLACES (états internes)
 * ──────────────────────────────────────────────────────────────────────────────
 *  QueenActor            │ queen_alive, queen_dead, queen_sated, hunger_token
 *  StorageActor          │ stock  (1 jeton = 1 unité, borné à 20)
 *  ForagerAntActor       │ forager_idle, forager_resting, forager_resting_waiting,
 *                        │ forager_dead, forager_starvation
 *  CarrierAntActor       │ carrier_idle, carrier_waiting, carrier_resting,
 *                        │ carrier_resting_waiting, carrier_dead, carrier_starvation
 *  EggActor              │ egg_cycle1, egg_cycle2  (2 places = 2 cycles d'incubation)
 *
 *  MESSAGE AKKA          │ TRANSITION
 * ──────────────────────────────────────────────────────────────────────────────
 *  Tick (reine)          │ t_tick, t_queen_dies
 *  Tick (ponte)          │ t_lay_egg
 *  Tick (œuf cycle 1)    │ t_egg_incubate
 *  Tick (œuf cycle 2)    │ t_spawn_forager | t_spawn_carrier
 *  FeedQueen(2)          │ t_carrier_deliver   (hunger -= 2)
 *  FeedQueen→sated       │ t_carrier_deliver_sated (hunger = 1 → 0)
 *  queen_sated → alive   │ t_queen_unsated
 *  AntDied(Forager)      │ t_forager_died_notify
 *  AntDied(Carrier)      │ t_carrier_died_notify
 *  SearchFood (forag.)   │ t_search_food  (inclut DepositFood)
 *  AntTick idle (forag.) │ t_forager_starve_idle, t_forager_dies_idle
 *  ConsumeFood (forag.)  │ t_forager_consume (ok), t_forager_consume_empty (vide)
 *  FoodReady (forag.)    │ t_forager_fed
 *  StorageEmpty (forag.) │ t_forager_starve_resting, t_forager_dies_resting
 *  SearchFood (carrier)  │ t_request_food (ok), t_request_food_empty (vide)
 *  FoodReady (carrier)   │ t_carrier_deliver, t_carrier_deliver_sated
 *  StorageEmpty (carr.)  │ t_carrier_storage_empty_idle
 *  AntTick idle (carr.)  │ t_carrier_starve_idle, t_carrier_dies_idle
 *  ConsumeFood (carr.)   │ t_carrier_consume, t_carrier_consume_empty
 *  FoodReady (carr.rest) │ t_carrier_fed
 *  StorageEmpty (c.rest) │ t_carrier_starve_resting, t_carrier_dies_resting
 */

// ══════════════════════════════════════════════════════════════════════════════
// Structures de données P/T pures
// ══════════════════════════════════════════════════════════════════════════════

/** Une place dans le réseau de Pétri, identifiée par un nom unique. */
case class Place(name: String)

/**
 * Une transition dans le réseau de Pétri P/T.
 *
 * @param name    Identifiant (correspond à un message Akka)
 * @param inputs  Places d'entrée → poids de consommation (entiers > 0)
 * @param outputs Places de sortie → poids de production (entiers > 0)
 * @param guard   Condition booléenne documentaire (hors sémantique P/T pure)
 */
case class Transition(
                       name:    String,
                       inputs:  Map[Place, Int],
                       outputs: Map[Place, Int],
                       guard:   String = ""
                     )

/**
 * Marquage M : association Place → nombre de jetons ≥ 0.
 * Représente l'état global du système à un instant donné.
 */
case class Marking(tokens: Map[Place, Int]) {

  def apply(p: Place): Int = tokens.getOrElse(p, 0)

  /** Vérifie si la transition t est franchissable (précondition P/T). */
  def enables(t: Transition): Boolean =
    t.inputs.forall { case (p, w) => this(p) >= w }

  /**
   * Franchit t et retourne le nouveau marquage.
   * Précondition : enables(t) == true.
   * Tous les poids sont ≥ 1 (P/T pur — pas de poids négatifs).
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
      .toList.sortBy(_._1.name)
      .map { case (p, n) => s"${p.name}=$n" }
      .mkString("{ ", ", ", " }")
}

/** Réseau de Pétri complet. */
case class PetriNet(
                     places:      Set[Place],
                     transitions: Set[Transition],
                     initial:     Marking
                   ) {

  def enabled(m: Marking): Set[Transition] = transitions.filter(m.enables)

  /**
   * Exploration BFS de l'espace d'états.
   * @return (marquages atteignables, arcs)
   */
  def reachabilityGraph(maxStates: Int = 2000)
  : (Set[Marking], Set[(Marking, Transition, Marking)]) = {
    import scala.collection.mutable
    val visited = mutable.Set[Marking](initial)
    val queue   = mutable.Queue[Marking](initial)
    val edges   = mutable.Set[(Marking, Transition, Marking)]()
    while (queue.nonEmpty && visited.size < maxStates) {
      val current = queue.dequeue()
      for (t <- enabled(current)) {
        val next = current.fire(t)
        edges += ((current, t, next))
        if (!visited.contains(next)) { visited += next; queue.enqueue(next) }
      }
    }
    (visited.toSet, edges.toSet)
  }

  /** Marquages deadlock dans l'espace atteignable. */
  def deadlocks(maxStates: Int = 2000): Set[Marking] = {
    val (reachable, _) = reachabilityGraph(maxStates)
    reachable.filter(m => enabled(m).isEmpty)
  }
}

// ══════════════════════════════════════════════════════════════════════════════
// Réseau de la colonie de fourmis
// ══════════════════════════════════════════════════════════════════════════════

object AntColonyPetriNet {

  // ─────────────────────────────────────────────────────────────────────────
  // PLACES
  // ─────────────────────────────────────────────────────────────────────────

  // Reine — exactement 1 jeton dans {queen_alive, queen_dead, queen_sated} (PI1)
  val queenAlive  = Place("queen_alive")
  val queenDead   = Place("queen_dead")
  val queenSated  = Place("queen_sated")
  // hunger_token : M(hunger_token) ∈ [0, 10] — 1 jeton = 1 point de faim
  val hungerToken = Place("hunger_token")

  // Stockage : M(stock) ∈ [0, 20]
  val stock = Place("stock")

  // Fourrageuse
  val foragerIdle           = Place("forager_idle")
  val foragerResting        = Place("forager_resting")
  val foragerRestingWaiting = Place("forager_resting_waiting")
  val foragerDead           = Place("forager_dead")
  val foragerStarvation     = Place("forager_starvation")  // [0, 3]

  // Transporteuse
  val carrierIdle           = Place("carrier_idle")
  val carrierWaiting        = Place("carrier_waiting")
  val carrierResting        = Place("carrier_resting")
  val carrierRestingWaiting = Place("carrier_resting_waiting")
  val carrierDead           = Place("carrier_dead")
  val carrierStarvation     = Place("carrier_starvation")  // [0, 3]

  // Œuf — modélisation correcte avec 2 places distinctes pour les 2 cycles
  // egg_cycle1 : cyclesLeft = 2  (œuf tout juste pondu)
  // egg_cycle2 : cyclesLeft = 1  (après 1 Tick)
  val eggCycle1 = Place("egg_cycle1")
  val eggCycle2 = Place("egg_cycle2")

  val allPlaces: Set[Place] = Set(
    queenAlive, queenDead, queenSated, hungerToken,
    stock,
    foragerIdle, foragerResting, foragerRestingWaiting, foragerDead, foragerStarvation,
    carrierIdle, carrierWaiting, carrierResting, carrierRestingWaiting, carrierDead, carrierStarvation,
    eggCycle1, eggCycle2
  )

  // ─────────────────────────────────────────────────────────────────────────
  // TRANSITIONS
  // ─────────────────────────────────────────────────────────────────────────

  // ══ REINE ══════════════════════════════════════════════════════════════════

  /**
   * t_tick : Tick → hunger + 1.
   * Akka : case Tick → val next = hunger + 1
   * Franchissable seulement si hunger_token <= 10 (garde ; sinon t_queen_dies).
   */
  val tTick = Transition(
    name    = "t_tick",
    inputs  = Map(queenAlive -> 1),
    outputs = Map(queenAlive -> 1, hungerToken -> 1),
    guard   = "M(hunger_token) <= MaxHunger=10"
  )

  /**
   * t_queen_dies : hunger atteint 11 → reine morte.
   * Akka : if (next > MaxHunger) → Behaviors.stopped
   * Consomme tous les 10 jetons hunger_token + queen_alive + jeton bonus → queen_dead.
   */
  val tQueenDies = Transition(
    name    = "t_queen_dies",
    inputs  = Map(queenAlive -> 1, hungerToken -> 11),
    outputs = Map(queenDead -> 1)
  )

  /**
   * t_lay_egg : à chaque Tick, la reine pond EggsPerCycle=2 œufs.
   * Akka : (1 to EggsPerCycle).map { i => context.spawn(EggActor(...)) }
   * Produit 2 jetons dans egg_cycle1 (cyclesLeft = 2).
   */
  val tLayEgg = Transition(
    name    = "t_lay_egg",
    inputs  = Map(queenAlive -> 1),
    outputs = Map(queenAlive -> 1, eggCycle1 -> 2)
  )

  // ══ ŒUF ════════════════════════════════════════════════════════════════════

  /**
   * t_egg_incubate : 1er cycle d'incubation (cyclesLeft : 2 → 1).
   * Akka : case Tick → val next = cyclesLeft - 1  (EggActor)
   * CORRECTION vs version précédente : transition propre egg_cycle1 → egg_cycle2,
   * pas une boucle no-op. Un jeton egg_cycle1 devient un jeton egg_cycle2.
   */
  val tEggIncubate = Transition(
    name    = "t_egg_incubate",
    inputs  = Map(eggCycle1 -> 1),
    outputs = Map(eggCycle2 -> 1)
  )

  /**
   * t_spawn_forager : 2e cycle → éclosion → naissance d'une fourrageuse.
   * Akka : if (next <= 0) → colony ! SpawnAnt(Forager) → context.spawn(supervisedForager)
   * Condition : foragerCount < MaxPerType=4 (garde documentaire).
   */
  val tSpawnForager = Transition(
    name    = "t_spawn_forager",
    inputs  = Map(eggCycle2 -> 1, queenAlive -> 1),
    outputs = Map(queenAlive -> 1, foragerIdle -> 1),
    guard   = "M(forager_idle)+M(forager_resting)+M(forager_resting_waiting) < MaxPerType=4"
  )

  /**
   * t_spawn_carrier : 2e cycle → éclosion → naissance d'une transporteuse.
   */
  val tSpawnCarrier = Transition(
    name    = "t_spawn_carrier",
    inputs  = Map(eggCycle2 -> 1, queenAlive -> 1),
    outputs = Map(queenAlive -> 1, carrierIdle -> 1),
    guard   = "M(carrier_idle)+M(carrier_waiting)+M(carrier_resting)+M(carrier_resting_waiting) < MaxPerType=4"
  )

  // ══ NOURRISSAGE DE LA REINE ════════════════════════════════════════════════

  /**
   * t_carrier_deliver : livraison de CarryCapacity=2 unités → hunger -= 2.
   * Akka : queen ! FeedQueen(amount) → val next = (hunger - amount) max 0
   * CORRECTION vs version précédente : consomme hunger_token (pas de poids négatif).
   * Le stock a déjà été prélevé dans t_request_food.
   */
  val tCarrierDeliver = Transition(
    name    = "t_carrier_deliver",
    inputs  = Map(carrierWaiting -> 1, hungerToken -> 2),
    outputs = Map(carrierResting -> 1),
    guard   = "M(hunger_token) >= 2"
  )

  /**
   * t_carrier_deliver_sated : cas limite hunger = 1 → hunger passe à 0 → queen_sated.
   * Akka : val next = (hunger - amount) max 0   (next = 0)
   */
  val tCarrierDeliverSated = Transition(
    name    = "t_carrier_deliver_sated",
    inputs  = Map(carrierWaiting -> 1, queenAlive -> 1, hungerToken -> 1),
    outputs = Map(carrierResting -> 1, queenSated -> 1)
  )

  /**
   * t_queen_unsated : la reine rassasiée (hunger = 0) reprend le cycle normal.
   * Akka : if (next == 0) context.log.info("[Reine] Totalement rassasiée.")
   */
  val tQueenUnsated = Transition(
    name    = "t_queen_unsated",
    inputs  = Map(queenSated -> 1),
    outputs = Map(queenAlive -> 1)
  )

  // ══ NOTIFICATIONS MORT ══════════════════════════════════════════════════════

  /**
   * t_forager_died_notify : AntDied(Forager) → QueenActor (foragerCount -= 1).
   * Akka : queen ! AntDied(Forager, id)
   * Le jeton forager_dead est consommé (fourmi retirée définitivement).
   */
  val tForagerDiedNotify = Transition(
    name    = "t_forager_died_notify",
    inputs  = Map(foragerDead -> 1, queenAlive -> 1),
    outputs = Map(queenAlive -> 1)
  )

  /**
   * t_carrier_died_notify : AntDied(Carrier) → QueenActor (carrierCount -= 1).
   */
  val tCarrierDiedNotify = Transition(
    name    = "t_carrier_died_notify",
    inputs  = Map(carrierDead -> 1, queenAlive -> 1),
    outputs = Map(queenAlive -> 1)
  )

  // ══ FOURRAGEUSE ═════════════════════════════════════════════════════════════

  /**
   * t_search_food : SearchFood → collecte + DepositFood(~2 unités).
   * Akka : val found = Random.nextInt(3)+1 ; storage ! DepositFood(found)
   * Poids 2 = moyenne de {1,2,3}. La fourrageuse passe idle → resting.
   * La famine est réinitialisée implicitement (starvation = 0 dans Akka).
   */
  val tSearchFood = Transition(
    name    = "t_search_food",
    inputs  = Map(foragerIdle -> 1),
    outputs = Map(foragerResting -> 1, stock -> 2),
    guard   = "M(stock) + 2 <= MaxCapacity=20"
  )

  /**
   * t_forager_starve_reset : purge de 1 jeton de famine après nourrissage.
   * Akka : starvation = 0 (lors de SearchFood ou FoodReady).
   * P/T pur : on consomme 1 jeton forager_starvation en présence de resting
   * (la fourrageuse vient d'être nourrie → elle est en resting).
   * Cette transition est appelée autant de fois qu'il y a de jetons de famine.
   */
  val tForagerStarveReset = Transition(
    name    = "t_forager_starve_reset",
    inputs  = Map(foragerStarvation -> 1, foragerResting -> 1),
    outputs = Map(foragerResting -> 1),
    guard   = "purge famine post-nourrissage (starvation → 0)"
  )

  /**
   * t_forager_starve_idle : AntTick en idle → famine +1.
   * Akka : case AntTick → val nextStarvation = starvation + 1
   */
  val tForagerStarveIdle = Transition(
    name    = "t_forager_starve_idle",
    inputs  = Map(foragerIdle -> 1),
    outputs = Map(foragerIdle -> 1, foragerStarvation -> 1),
    guard   = "M(forager_starvation) < MaxStarvation=3"
  )

  /**
   * t_forager_dies_idle : mort par famine en idle (starvation = 3).
   * Akka : if (nextStarvation >= MaxStarvation) → queen ! AntDied ; Behaviors.stopped
   * Modélisation P/T : consomme exactement 3 jetons forager_starvation.
   */
  val tForagerDiesIdle = Transition(
    name    = "t_forager_dies_idle",
    inputs  = Map(foragerIdle -> 1, foragerStarvation -> 3),
    outputs = Map(foragerDead -> 1)
  )

  /**
   * t_forager_consume : AntTick resting + stock ≥ 1 → ConsumeFood → FoodReady.
   * Akka : storage ! ConsumeFood(self) → stock -= 1 → FoodReady(1)
   */
  val tForagerConsume = Transition(
    name    = "t_forager_consume",
    inputs  = Map(foragerResting -> 1, stock -> 1),
    outputs = Map(foragerRestingWaiting -> 1)
  )

  /**
   * t_forager_consume_empty : AntTick resting + stock = 0 → StorageEmpty.
   * Garde documentaire (P/T pur ne peut tester "stock = 0" sans inhibiteur).
   */
  val tForagerConsumeEmpty = Transition(
    name    = "t_forager_consume_empty",
    inputs  = Map(foragerResting -> 1),
    outputs = Map(foragerRestingWaiting -> 1),
    guard   = "M(stock) = 0  [StorageEmpty]"
  )

  /**
   * t_forager_fed : FoodReady → fourrageuse nourrie → idle.
   * Akka : case FoodReady(_) → starvation = 0 ; si restCycles ≤ 0 → idle
   */
  val tForagerFed = Transition(
    name    = "t_forager_fed",
    inputs  = Map(foragerRestingWaiting -> 1),
    outputs = Map(foragerIdle -> 1)
  )

  /**
   * t_forager_starve_resting : StorageEmpty en resting_waiting → famine +1.
   * Akka : case StorageEmpty → nextStarvation + 1 → resting (si < max)
   */
  val tForagerStarveResting = Transition(
    name    = "t_forager_starve_resting",
    inputs  = Map(foragerRestingWaiting -> 1),
    outputs = Map(foragerResting -> 1, foragerStarvation -> 1),
    guard   = "M(forager_starvation) < MaxStarvation=3"
  )

  /**
   * t_forager_dies_resting : mort par famine en resting_waiting.
   */
  val tForagerDiesResting = Transition(
    name    = "t_forager_dies_resting",
    inputs  = Map(foragerRestingWaiting -> 1, foragerStarvation -> 3),
    outputs = Map(foragerDead -> 1)
  )

  // ══ TRANSPORTEUSE ═══════════════════════════════════════════════════════════

  /**
   * t_request_food : SearchFood + stock ≥ CarryCapacity=2 → RequestFood accepté.
   * Akka : storage ! RequestFood(2, self) → stock -= 2 → FoodReady → carrier_waiting
   */
  val tRequestFood = Transition(
    name    = "t_request_food",
    inputs  = Map(carrierIdle -> 1, stock -> 2),
    outputs = Map(carrierWaiting -> 1)
  )

  /**
   * t_request_food_empty : SearchFood + stock < 2 → StorageEmpty → retour idle.
   * Akka : case StorageEmpty → idle(...)
   * Garde documentaire (même limite P/T que ci-dessus).
   */
  val tRequestFoodEmpty = Transition(
    name    = "t_request_food_empty",
    inputs  = Map(carrierIdle -> 1),
    outputs = Map(carrierIdle -> 1),
    guard   = "M(stock) < CarryCapacity=2  [StorageEmpty → retour idle]"
  )

  /**
   * t_carrier_storage_empty_idle : StorageEmpty reçu en carrier_waiting → retour idle.
   * (cas où le stock est passé à 0 entre la demande et la réponse)
   */
  val tCarrierStorageEmptyIdle = Transition(
    name    = "t_carrier_storage_empty_idle",
    inputs  = Map(carrierWaiting -> 1),
    outputs = Map(carrierIdle -> 1),
    guard   = "M(stock) = 0  [StorageEmpty reçu]"
  )

  /** t_carrier_starve_idle : AntTick idle → famine +1. */
  val tCarrierStarveIdle = Transition(
    name    = "t_carrier_starve_idle",
    inputs  = Map(carrierIdle -> 1),
    outputs = Map(carrierIdle -> 1, carrierStarvation -> 1),
    guard   = "M(carrier_starvation) < MaxStarvation=3"
  )

  /** t_carrier_dies_idle : mort par famine en idle (starvation = 3). */
  val tCarrierDiesIdle = Transition(
    name    = "t_carrier_dies_idle",
    inputs  = Map(carrierIdle -> 1, carrierStarvation -> 3),
    outputs = Map(carrierDead -> 1)
  )

  /** t_carrier_consume : AntTick resting + stock ≥ 1 → FoodReady. */
  val tCarrierConsume = Transition(
    name    = "t_carrier_consume",
    inputs  = Map(carrierResting -> 1, stock -> 1),
    outputs = Map(carrierRestingWaiting -> 1)
  )

  /** t_carrier_consume_empty : AntTick resting + stock = 0 → StorageEmpty. */
  val tCarrierConsumeEmpty = Transition(
    name    = "t_carrier_consume_empty",
    inputs  = Map(carrierResting -> 1),
    outputs = Map(carrierRestingWaiting -> 1),
    guard   = "M(stock) = 0  [StorageEmpty]"
  )

  /** t_carrier_fed : FoodReady → transporteuse nourrie → idle. */
  val tCarrierFed = Transition(
    name    = "t_carrier_fed",
    inputs  = Map(carrierRestingWaiting -> 1),
    outputs = Map(carrierIdle -> 1)
  )

  /** t_carrier_starve_resting : StorageEmpty → famine +1 en resting_waiting. */
  val tCarrierStarveResting = Transition(
    name    = "t_carrier_starve_resting",
    inputs  = Map(carrierRestingWaiting -> 1),
    outputs = Map(carrierResting -> 1, carrierStarvation -> 1),
    guard   = "M(carrier_starvation) < MaxStarvation=3"
  )

  /** t_carrier_dies_resting : mort par famine en resting_waiting. */
  val tCarrierDiesResting = Transition(
    name    = "t_carrier_dies_resting",
    inputs  = Map(carrierRestingWaiting -> 1, carrierStarvation -> 3),
    outputs = Map(carrierDead -> 1)
  )

  /** t_carrier_starve_reset : purge de 1 jeton famine après nourrissage. */
  val tCarrierStarveReset = Transition(
    name    = "t_carrier_starve_reset",
    inputs  = Map(carrierStarvation -> 1, carrierResting -> 1),
    outputs = Map(carrierResting -> 1),
    guard   = "purge famine post-nourrissage (starvation → 0)"
  )

  // ─────────────────────────────────────────────────────────────────────────
  val allTransitions: Set[Transition] = Set(
    tTick, tQueenDies, tLayEgg,
    tCarrierDeliver, tCarrierDeliverSated, tQueenUnsated,
    tEggIncubate, tSpawnForager, tSpawnCarrier,
    tForagerDiedNotify, tCarrierDiedNotify,
    tSearchFood, tForagerStarveReset,
    tForagerStarveIdle, tForagerDiesIdle,
    tForagerConsume, tForagerConsumeEmpty,
    tForagerFed, tForagerStarveResting, tForagerDiesResting,
    tRequestFood, tRequestFoodEmpty, tCarrierStorageEmptyIdle,
    tCarrierStarveIdle, tCarrierDiesIdle,
    tCarrierConsume, tCarrierConsumeEmpty,
    tCarrierFed, tCarrierStarveResting, tCarrierDiesResting,
    tCarrierStarveReset
  )

  // ─────────────────────────────────────────────────────────────────────────
  // MARQUAGE INITIAL M₀ — correspond exactement à Simulation.scala
  // ─────────────────────────────────────────────────────────────────────────
  val initialMarking: Marking = Marking(Map(
    queenAlive  -> 1,
    hungerToken -> 5,   // initialHunger = 5
    foragerIdle -> 1,
    carrierIdle -> 1
    // stock = 0, eggCycle1 = 0, eggCycle2 = 0 (implicites)
  ))

  val net: PetriNet = PetriNet(allPlaces, allTransitions, initialMarking)

  // ══════════════════════════════════════════════════════════════════════════
  // P-INVARIANTS
  // ══════════════════════════════════════════════════════════════════════════
  /**
   * Un P-invariant est un vecteur y ≥ 0 tel que y^T · C = 0
   * où C = Post - Pre est la matrice d'incidence.
   * Conséquence : ∀ M atteignable, y^T · M = y^T · M₀ (quantité conservée).
   *
   * PI1 — Conservation de l'état de la reine :
   *   y = (queen_alive:1, queen_dead:1, queen_sated:1, autres:0)
   *   M(queen_alive) + M(queen_dead) + M(queen_sated) = 1  ∀ M
   *   → Lié aux invariants métier [I7] de Invariants.scala.
   *
   * PI2 — Borne de faim :
   *   0 ≤ M(hunger_token) ≤ 10
   *   → [I1] (faim ≥ 0) et [I2] (faim ≤ MaxHunger).
   *
   * PI3 — Conservation fourrageuse :
   *   M(forager_idle) + M(forager_resting) + M(forager_resting_waiting) + M(forager_dead)
   *     ∈ [0, 4]
   *   → [I5].
   *
   * PI4 — Conservation transporteuse :
   *   M(carrier_idle) + M(carrier_waiting) + M(carrier_resting)
   *     + M(carrier_resting_waiting) + M(carrier_dead) ∈ [0, 4]
   *   → [I6].
   *
   * PI5 — Borne du stock :
   *   0 ≤ M(stock) ≤ 20
   *   → [I3] et [I4].
   *
   * PI6 — Cohérence vivacité reine :
   *   M(queen_alive) = 1 → M(hunger_token) ≤ 10
   *   → [I7].
   */
  def checkPInvariants(m: Marking): List[String] = {
    val v = scala.collection.mutable.ListBuffer[String]()

    val queenSum = m(queenAlive) + m(queenDead) + m(queenSated)
    if (queenSum != 1)
      v += s"[PI1] VIOLATION : reine dans $queenSum états (attendu 1)"

    val h = m(hungerToken)
    if (h < 0)  v += s"[PI2] VIOLATION : faim négative ($h) — [I1]"
    // On autorise 11 car c'est la valeur qui déclenche la mort
    if (h > 11) v += s"[PI2] VIOLATION : faim $h > 11 — [I2]"

    val ft = m(foragerIdle) + m(foragerResting) + m(foragerRestingWaiting) + m(foragerDead)
    if (ft > 4) v += s"[PI3] VIOLATION : $ft fourrageuses > MaxPerType=4 — [I5]"

    val ct = m(carrierIdle) + m(carrierWaiting) + m(carrierResting) + m(carrierRestingWaiting) + m(carrierDead)
    if (ct > 4) v += s"[PI4] VIOLATION : $ct transporteuses > MaxPerType=4 — [I6]"

    if (m(stock) < 0)  v += s"[PI5] VIOLATION : stock négatif — [I3]"
    if (m(stock) > 20) v += s"[PI5] VIOLATION : stock ${m(stock)} > MaxCapacity=20 — [I4]"

    if (m(queenAlive) == 1 && h > 11)
      v += s"[PI6] VIOLATION : reine vivante avec faim=$h > 11 — [I7]"

    v.toList
  }

  // ══════════════════════════════════════════════════════════════════════════
  // MATRICE D'INCIDENCE + VÉRIFICATION ANALYTIQUE P-INVARIANT
  // ══════════════════════════════════════════════════════════════════════════

  /** C[p][t] = Post(p,t) - Pre(p,t) */
  def incidenceMatrix: Map[Place, Map[Transition, Int]] =
    allPlaces.map { p =>
      p -> allTransitions.map { t =>
        t -> (t.outputs.getOrElse(p, 0) - t.inputs.getOrElse(p, 0))
      }.toMap
    }.toMap

  /**
   * Vérifie si y est un P-invariant : ∀ t, ∑_p y(p) · C[p][t] = 0.
   * Permet la vérification formelle des P-invariants identifiés.
   */
  def isPInvariant(y: Map[Place, Int]): Boolean =
    allTransitions.forall { t =>
      allPlaces.toList.map(p => y.getOrElse(p, 0) * (t.outputs.getOrElse(p, 0) - t.inputs.getOrElse(p, 0))).sum == 0
    }

  // ══════════════════════════════════════════════════════════════════════════
  // T-INVARIANTS
  // ══════════════════════════════════════════════════════════════════════════
  /**
   * Un T-invariant est un vecteur x ≥ 0 tel que C · x = 0.
   * Il représente un multiensemble de tirs qui ramène M à M₀.
   *
   * TI1 — Cycle nominal fourrageuse :
   *   t_search_food + t_forager_consume + t_forager_fed
   *   Bilan sur forager_idle : -1+1 = 0 ✓  stock : +2-1=+1 (pas conservé → semi-flot)
   *
   * TI2 — Cycle nominal transporteuse :
   *   t_request_food + t_carrier_deliver + t_carrier_consume + t_carrier_fed
   *   Bilan carrier_idle : 0 ✓  hunger_token : -2 ✓  stock : -2-1 = -3
   *
   * TI3 — Cycle faim reine :
   *   t_tick + t_carrier_deliver
   *   hunger_token net : +1 - 2 = -1 (flux de réduction de faim)
   *
   * TI4 — Cycle vie d'un œuf :
   *   t_lay_egg + t_egg_incubate + t_spawn_forager
   *   eggCycle1 : +2-1=+1 ; eggCycle2 : +1-1=0 ; forager_idle : +1
   */
  val tInvariants: Map[String, String] = Map(
    "TI1" -> "t_search_food · t_forager_consume · t_forager_fed  [cycle fourrageuse]",
    "TI2" -> "t_request_food · t_carrier_deliver · t_carrier_consume · t_carrier_fed  [cycle transporteuse]",
    "TI3" -> "t_tick · t_carrier_deliver  [cycle faim reine]",
    "TI4" -> "t_lay_egg · t_egg_incubate · t_spawn_forager  [cycle œuf → fourrageuse]"
  )

  // ══════════════════════════════════════════════════════════════════════════
  // PROPRIÉTÉS LTL ET VÉRIFICATEURS
  // ══════════════════════════════════════════════════════════════════════════
  /**
   * LTL (Linear Temporal Logic) : logique pour exprimer des propriétés
   * sur les CHEMINS d'exécution (séquences de marquages).
   * Opérateurs : □ "toujours", ◇ "un jour", ○ "prochain", U "jusqu'à", ¬ "non".
   *
   * ── Sûreté (□ P) ─────────────────────────────────────────────────────────
   *
   * LTL1 □ (M(queen_alive)+M(queen_dead)+M(queen_sated) = 1)
   *   La reine est toujours dans un état bien défini. [PI1]
   *
   * LTL2 □ (M(stock) ≥ 0)
   *   Le stock ne devient jamais négatif. [PI5]
   *
   * LTL3 □ (population(F) ≤ 4 ∧ population(C) ≤ 4)
   *   La population est toujours bornée. [PI3][PI4]
   *
   * LTL4 □ (M(queen_alive)=1 → 0 ≤ M(hunger_token) ≤ 10)
   *   La faim reste bornée si la reine est vivante. [PI2][PI6]
   *
   * ── Vivacité (◇ P depuis tout état) ──────────────────────────────────────
   *
   * LTL5 □ (M(forager_idle) ≥ 1 → ◇ t_search_food franchissable)
   *   Toute fourrageuse idle finit par chercher de la nourriture.
   *
   * LTL6 □ (M(carrier_idle) ≥ 1 ∧ M(stock) ≥ 2 → ◇ t_carrier_deliver franchissable)
   *   Toute transporteuse avec stock disponible finit par livrer.
   *
   * LTL7 □ (M(egg_cycle1) ≥ 1 → ◇ t_egg_incubate franchissable)
   *   Tout œuf en cycle 1 progresse vers le cycle 2.
   *
   * ── Terminaison / Deadlock ────────────────────────────────────────────────
   *
   * LTL8 □ (deadlock → M(queen_dead) = 1)
   *   Un deadlock n'est atteignable que si la reine est morte.
   *
   * LTL9 □ ((fourmis_actives = 0 ∧ M(stock) = 0) → ◇ M(queen_dead) = 1)
   *   Sans fourmis ni stock, la reine meurt inévitablement.
   */
  val ltlFormulas: Map[String, String] = Map(
    "LTL1" -> "□ (M(queen_alive)+M(queen_dead)+M(queen_sated) = 1)  [sûreté état reine]",
    "LTL2" -> "□ (M(stock) ≥ 0)  [sûreté stock]",
    "LTL3" -> "□ (population(F) ≤ MaxPerType=4 ∧ population(C) ≤ MaxPerType=4)  [sûreté population]",
    "LTL4" -> "□ (M(queen_alive)=1 → 0 ≤ M(hunger_token) ≤ 10)  [sûreté faim]",
    "LTL5" -> "□ (M(forager_idle)≥1 → ◇ t_search_food franchissable)  [vivacité fourrageuse]",
    "LTL6" -> "□ (M(carrier_idle)≥1 ∧ M(stock)≥2 → ◇ t_carrier_deliver franchissable)  [vivacité transporteuse]",
    "LTL7" -> "□ (M(egg_cycle1)≥1 → ◇ t_egg_incubate franchissable)  [vivacité œuf]",
    "LTL8" -> "□ (deadlock → M(queen_dead)=1)  [deadlock = fin de colonie]",
    "LTL9" -> "□ (fourmis_actives=0 ∧ M(stock)=0 → ◇ M(queen_dead)=1)  [terminaison inévitable]"
  )

  // ── Vérificateurs sur l'espace d'états ────────────────────────────────────

  /** LTL1 : PI1 — reine dans exactement 1 état. */
  def verifyLTL1(r: Set[Marking]): Boolean =
    r.forall(m => m(queenAlive) + m(queenDead) + m(queenSated) == 1)

  /** LTL2 : PI5 — stock toujours ≥ 0. */
  def verifyLTL2(r: Set[Marking]): Boolean =
    r.forall(m => m(stock) >= 0)

  /** LTL3 : PI3 + PI4 — population bornée. */
  def verifyLTL3(r: Set[Marking]): Boolean =
    r.forall { m =>
      val f = m(foragerIdle) + m(foragerResting) + m(foragerRestingWaiting) + m(foragerDead)
      val c = m(carrierIdle) + m(carrierWaiting) + m(carrierResting) + m(carrierRestingWaiting) + m(carrierDead)
      f <= 4 && c <= 4
    }

  /** LTL4 : PI2 + PI6 — faim bornée si reine vivante. */
  def verifyLTL4(r: Set[Marking]): Boolean =
    r.forall(m => m(queenAlive) != 1 || (m(hungerToken) >= 0 && m(hungerToken) <= 11))

  /**
   * LTL5 : vivacité fourrageuse.
   * t_search_food ne dépend que de forager_idle → franchissable directement.
   */
  def verifyLTL5(r: Set[Marking]): Boolean =
    r.forall(m => m(foragerIdle) == 0 || m.enables(tSearchFood))

  /**
   * LTL6 : vivacité transporteuse.
   * t_request_food nécessite carrier_idle + 2 stock.
   */
  def verifyLTL6(r: Set[Marking]): Boolean =
    r.forall(m => !(m(carrierIdle) >= 1 && m(stock) >= 2) || m.enables(tRequestFood))

  /**
   * LTL7 : vivacité œuf.
   * t_egg_incubate nécessite 1 jeton dans egg_cycle1.
   */
  def verifyLTL7(r: Set[Marking]): Boolean =
    r.forall(m => m(eggCycle1) == 0 || m.enables(tEggIncubate))

  /** LTL8 : deadlock uniquement si queen_dead. */
  def verifyLTL8(dl: Set[Marking]): Boolean =
    dl.forall(m => m(queenDead) == 1)

  /**
   * LTL9 : terminaison inévitable sans fourmis actives et sans stock.
   * Dans ces marquages, t_tick est franchissable → hunger_token montera → queen_dead.
   */
  def verifyLTL9(r: Set[Marking]): Boolean =
    r.forall { m =>
      val af = m(foragerIdle) + m(foragerResting) + m(foragerRestingWaiting)
      val ac = m(carrierIdle) + m(carrierWaiting) + m(carrierResting) + m(carrierRestingWaiting)
      !(af == 0 && ac == 0 && m(stock) == 0 && m(queenAlive) == 1) || m.enables(tTick)
    }
}

// ══════════════════════════════════════════════════════════════════════════════
// Analyseur — rapport complet
// ══════════════════════════════════════════════════════════════════════════════

object PetriNetMain {
  def main(args: Array[String]): Unit = {
    import AntColonyPetriNet._
    val sep = "=" * 72

    println(sep)
    println("  RÉSEAU DE PÉTRI — COLONIE DE FOURMIS (Akka/Scala)")
    println(sep)

    println(s"\n── 1. Structure")
    println(s"  Places      : ${allPlaces.size}")
    println(s"  Transitions : ${allTransitions.size}")
    println(s"  Marquage M₀ : ${net.initial}")

    val enabledInit = net.enabled(net.initial)
    println(s"\n── 2. Transitions franchissables depuis M₀ (${enabledInit.size})")
    enabledInit.toList.sortBy(_.name).foreach(t => println(s"  ✓ ${t.name}"))

    val maxS = 2000
    println(s"\n── 3. Espace d'états (BFS, max $maxS)")
    val (reachable, edges) = net.reachabilityGraph(maxS)
    val reached = if (reachable.size >= maxS) s"≥ $maxS (limite atteinte)" else s"${reachable.size}"
    println(s"  Marquages atteignables : $reached")
    println(s"  Arcs                   : ${edges.size}")

    val dl = net.deadlocks(maxS)
    println(s"\n── 4. Deadlocks : ${dl.size}")
    dl.take(3).foreach(m => println(s"  • $m"))

    println(s"\n── 5. P-invariants sur M₀")
    val piV0 = checkPInvariants(net.initial)
    if (piV0.isEmpty) println("  ✓ Tous satisfaits.")
    else piV0.foreach(v => println(s"  ✗ $v"))

    println(s"\n── 6. P-invariants sur l'espace d'états")
    val allPIV = reachable.flatMap(checkPInvariants)
    if (allPIV.isEmpty) println("  ✓ Aucune violation.")
    else { println(s"  ${allPIV.size} violation(s) :"); allPIV.take(5).foreach(v => println(s"  ✗ $v")) }

    println(s"\n── 7. Vérification analytique PI1 (y^T · C = 0)")
    val yPI1 = Map(queenAlive -> 1, queenDead -> 1, queenSated -> 1)
    println(s"  PI1 : ${if (isPInvariant(yPI1)) "✓ vérifié" else "✗ non vérifié"}")

    println(s"\n── 8. Vérification LTL sur l'espace d'états")
    def ok(b: Boolean) = if (b) "✓ OK" else "✗ VIOLATION"
    println(s"  LTL1 (état reine)       : ${ok(verifyLTL1(reachable))}")
    println(s"  LTL2 (stock ≥ 0)        : ${ok(verifyLTL2(reachable))}")
    println(s"  LTL3 (population bornée): ${ok(verifyLTL3(reachable))}")
    println(s"  LTL4 (faim bornée)      : ${ok(verifyLTL4(reachable))}")
    println(s"  LTL5 (vivacité forag.)  : ${ok(verifyLTL5(reachable))}")
    println(s"  LTL6 (vivacité transp.) : ${ok(verifyLTL6(reachable))}")
    println(s"  LTL7 (vivacité œuf)     : ${ok(verifyLTL7(reachable))}")
    println(s"  LTL8 (deadlock→dead)    : ${ok(verifyLTL8(dl))}")
    println(s"  LTL9 (terminaison)      : ${ok(verifyLTL9(reachable))}")

    println(s"\n── 9. Formules LTL")
    ltlFormulas.toList.sortBy(_._1).foreach { case (id, f) => println(s"  $id : $f") }

    println(s"\n── 10. T-invariants")
    tInvariants.toList.sortBy(_._1).foreach { case (id, d) => println(s"  $id : $d") }

    println(s"\n$sep")
  }
}