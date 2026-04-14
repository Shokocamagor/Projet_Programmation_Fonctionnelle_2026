/**
 * PetriNetSpec.scala — Tests unitaires du réseau de Pétri (colonie de fourmis)
 *
 * Framework : ScalaTest (FlatSpec + Matchers)
 * Dépendance build.sbt :
 *   libraryDependencies += "org.scalatest" %% "scalatest" % "3.2.17" % Test
 *
 * Couvre :
 *   1.  Structure du réseau (places, transitions, unicité)
 *   2.  Marquage initial M₀ (correspondance exacte Simulation.scala)
 *   3.  Franchissabilité depuis M₀
 *   4.  Franchissement correct (fire) — vérification des marquages résultants
 *   5.  P-invariants : vérification sur M₀ et détection de violations
 *   6.  Vérification analytique des P-invariants (y^T · C = 0)
          *   7.  Exploration BFS de l'espace d'états
          *   8.  Détection de deadlocks
          *   9.  Vérification LTL1–LTL9 sur l'espace d'états
          *   10. Scénarios de bout en bout (flux nominaux et de famine)
          *   11. Cycle de vie d'un œuf (2 places distinctes : correction vs version précédente)
          *   12. Correspondance Akka ↔ transitions

 **/

import AntColonyPetriNet._
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class PetriNetSpec extends AnyFlatSpec with Matchers {

  // Espace d'états pré-calculé (partagé entre les tests)
  lazy val (reachable, edges) = net.reachabilityGraph(5000)
  lazy val deadlockMarkings   = net.deadlocks(5000)

  // ══════════════════════════════════════════════════════════════════════════
  // 1. Structure du réseau
  // ══════════════════════════════════════════════════════════════════════════

  "Le réseau de Pétri" should "contenir toutes les places requises" in {
    allPlaces should contain allOf (
      queenAlive, queenDead, queenSated, hungerToken,
      stock,
      foragerIdle, foragerResting, foragerRestingWaiting, foragerDead, foragerStarvation,
      carrierIdle, carrierWaiting, carrierResting, carrierRestingWaiting, carrierDead, carrierStarvation,
      eggCycle1, eggCycle2
    )
  }

  it should "avoir exactement 18 places" in {
    allPlaces.size shouldBe 18
  }

  it should "contenir toutes les transitions critiques" in {
    val names = allTransitions.map(_.name)
    names should contain allOf (
      "t_tick", "t_queen_dies", "t_lay_egg",
      "t_egg_incubate", "t_spawn_forager", "t_spawn_carrier",
      "t_carrier_deliver", "t_carrier_deliver_sated", "t_queen_unsated",
      "t_forager_died_notify", "t_carrier_died_notify",
      "t_search_food",
      "t_forager_starve_idle", "t_forager_dies_idle",
      "t_forager_consume", "t_forager_fed",
      "t_forager_starve_resting", "t_forager_dies_resting",
      "t_request_food",
      "t_carrier_starve_idle", "t_carrier_dies_idle",
      "t_carrier_consume", "t_carrier_fed",
      "t_carrier_starve_resting", "t_carrier_dies_resting"
    )
  }

  it should "avoir des noms de transitions uniques" in {
    val names = allTransitions.map(_.name).toList
    names.distinct.size shouldBe names.size
  }

  it should "n'avoir aucune transition avec un poids de sortie négatif (P/T pur)" in {
    allTransitions.foreach { t =>
      t.outputs.values.foreach(w => w should be > 0)
    }
  }

  it should "n'avoir aucune transition avec un poids d'entrée négatif (P/T pur)" in {
    allTransitions.foreach { t =>
      t.inputs.values.foreach(w => w should be > 0)
    }
  }

  // ══════════════════════════════════════════════════════════════════════════
  // 2. Marquage initial M₀
  // ══════════════════════════════════════════════════════════════════════════

  "Le marquage initial M₀" should "correspondre exactement à Simulation.scala" in {
    initialMarking(queenAlive)  shouldBe 1
    initialMarking(hungerToken) shouldBe 5   // initialHunger = 5
    initialMarking(stock)       shouldBe 0   // initialStock = 0
    initialMarking(foragerIdle) shouldBe 1
    initialMarking(carrierIdle) shouldBe 1
    // Pas de morts, famine, œufs au démarrage
    initialMarking(foragerDead)       shouldBe 0
    initialMarking(carrierDead)       shouldBe 0
    initialMarking(foragerStarvation) shouldBe 0
    initialMarking(carrierStarvation) shouldBe 0
    initialMarking(eggCycle1)         shouldBe 0
    initialMarking(eggCycle2)         shouldBe 0
  }

  it should "satisfaire tous les P-invariants" in {
    checkPInvariants(initialMarking) shouldBe empty
  }

  // ══════════════════════════════════════════════════════════════════════════
  // 3. Franchissabilité depuis M₀
  // ══════════════════════════════════════════════════════════════════════════

  "Depuis M₀" should "permettre t_tick" in {
    initialMarking.enables(tTick) shouldBe true
  }

  it should "permettre t_lay_egg" in {
    initialMarking.enables(tLayEgg) shouldBe true
  }

  it should "permettre t_search_food" in {
    initialMarking.enables(tSearchFood) shouldBe true
  }

  it should "permettre t_forager_starve_idle" in {
    initialMarking.enables(tForagerStarveIdle) shouldBe true
  }

  it should "permettre t_carrier_starve_idle" in {
    initialMarking.enables(tCarrierStarveIdle) shouldBe true
  }

  it should "NE PAS permettre t_queen_dies (hunger_token = 5 < 10)" in {
    initialMarking.enables(tQueenDies) shouldBe false
  }

  it should "NE PAS permettre t_request_food (stock = 0 < CarryCapacity=2)" in {
    initialMarking.enables(tRequestFood) shouldBe false
  }

  it should "NE PAS permettre t_carrier_deliver (pas de carrierWaiting)" in {
    initialMarking.enables(tCarrierDeliver) shouldBe false
  }

  it should "NE PAS permettre t_forager_dies_idle (famine = 0 < 3)" in {
    initialMarking.enables(tForagerDiesIdle) shouldBe false
  }

  it should "NE PAS permettre t_egg_incubate (aucun œuf)" in {
    initialMarking.enables(tEggIncubate) shouldBe false
  }

  it should "NE PAS permettre t_spawn_forager (aucun œuf en cycle 2)" in {
    initialMarking.enables(tSpawnForager) shouldBe false
  }

  // ══════════════════════════════════════════════════════════════════════════
  // 4. Franchissement (fire)
  // ══════════════════════════════════════════════════════════════════════════

  "fire(t_tick)" should "augmenter hunger_token de 1 et conserver queen_alive" in {
    val m1 = initialMarking.fire(tTick)
    m1(hungerToken) shouldBe 6
    m1(queenAlive)  shouldBe 1
  }

  "fire(t_lay_egg)" should "produire 2 jetons dans egg_cycle1" in {
    val m1 = initialMarking.fire(tLayEgg)
    m1(eggCycle1) shouldBe 2
    m1(queenAlive) shouldBe 1
  }

  "fire(t_egg_incubate)" should "convertir egg_cycle1 en egg_cycle2 (transition propre)" in {
    // Correction clé : pas de boucle, la transition avance le cycle
    val mEgg = Marking(initialMarking.tokens + (eggCycle1 -> 1))
    mEgg.enables(tEggIncubate) shouldBe true
    val m1 = mEgg.fire(tEggIncubate)
    m1(eggCycle1) shouldBe 0   // consommé
    m1(eggCycle2) shouldBe 1   // produit
  }

  "fire(t_search_food)" should "passer forager idle→resting et déposer 2 stock" in {
    val m1 = initialMarking.fire(tSearchFood)
    m1(foragerIdle)    shouldBe 0
    m1(foragerResting) shouldBe 1
    m1(stock)          shouldBe 2
  }

  "fire(t_forager_starve_idle)" should "incrémenter forager_starvation de 1" in {
    val m1 = initialMarking.fire(tForagerStarveIdle)
    m1(foragerIdle)       shouldBe 1
    m1(foragerStarvation) shouldBe 1
  }

  "fire(t_request_food) avec stock=4" should "passer carrier idle→waiting et réduire stock de 2" in {
    val mStock = Marking(initialMarking.tokens + (stock -> 4))
    mStock.enables(tRequestFood) shouldBe true
    val m1 = mStock.fire(tRequestFood)
    m1(carrierIdle)    shouldBe 0
    m1(carrierWaiting) shouldBe 1
    m1(stock)          shouldBe 2
  }

  "fire(t_carrier_deliver) avec hunger_token=6" should "réduire hunger de 2 et passer carrier en resting" in {
    val mReady = Marking(Map(
      queenAlive    -> 1,
      hungerToken   -> 6,
      carrierWaiting -> 1,
      foragerIdle    -> 1
    ))
    mReady.enables(tCarrierDeliver) shouldBe true
    val m1 = mReady.fire(tCarrierDeliver)
    m1(carrierWaiting)  shouldBe 0
    m1(carrierResting)  shouldBe 1
    m1(hungerToken)     shouldBe 4   // 6 - 2
    m1(queenAlive)      shouldBe 1   // inchangé
  }

  "fire(t_carrier_deliver_sated) avec hunger_token=1" should "rassasier la reine" in {
    val mSated = Marking(Map(
      queenAlive    -> 1,
      hungerToken   -> 1,
      carrierWaiting -> 1
    ))
    mSated.enables(tCarrierDeliverSated) shouldBe true
    val m1 = mSated.fire(tCarrierDeliverSated)
    m1(queenAlive)  shouldBe 0
    m1(queenSated)  shouldBe 1
    m1(hungerToken) shouldBe 0
    m1(carrierResting) shouldBe 1
  }

  "fire(t_queen_unsated)" should "ramener queen_sated vers queen_alive" in {
    val mSated = Marking(Map(queenSated -> 1))
    mSated.enables(tQueenUnsated) shouldBe true
    val m1 = mSated.fire(tQueenUnsated)
    m1(queenSated) shouldBe 0
    m1(queenAlive) shouldBe 1
  }

  "fire(t_queen_dies) avec hunger_token=11" should "tuer la reine" in {
    val mFull = Marking(Map(queenAlive -> 1, hungerToken -> 11, foragerIdle -> 1, carrierIdle -> 1))
    mFull.enables(tQueenDies) shouldBe true
    mFull.enables(tTick)      shouldBe false   // tTick ne doit plus être possible à 11
    val m1 = mFull.fire(tQueenDies)
    m1(queenAlive)  shouldBe 0
    m1(queenDead)   shouldBe 1
  }

  "fire(t_forager_dies_idle) avec 3 jetons de famine" should "tuer la fourrageuse" in {
    val mStarved = Marking(Map(
      queenAlive -> 1, hungerToken -> 5,
      foragerIdle -> 1, foragerStarvation -> 3, carrierIdle -> 1
    ))
    mStarved.enables(tForagerDiesIdle) shouldBe true
    val m1 = mStarved.fire(tForagerDiesIdle)
    m1(foragerIdle)       shouldBe 0
    m1(foragerDead)       shouldBe 1
    m1(foragerStarvation) shouldBe 0
  }

  "fire(t_spawn_forager) avec egg_cycle2" should "créer une fourrageuse" in {
    val mEgg = Marking(Map(queenAlive -> 1, hungerToken -> 5, eggCycle2 -> 1, carrierIdle -> 1))
    mEgg.enables(tSpawnForager) shouldBe true
    val m1 = mEgg.fire(tSpawnForager)
    m1(eggCycle2)   shouldBe 0
    m1(foragerIdle) shouldBe 1
    m1(queenAlive)  shouldBe 1
  }

  "fire(t_forager_died_notify)" should "consommer forager_dead sans affecter queen_alive" in {
    val m = Marking(Map(queenAlive -> 1, hungerToken -> 5, foragerDead -> 1, carrierIdle -> 1))
    m.enables(tForagerDiedNotify) shouldBe true
    val m1 = m.fire(tForagerDiedNotify)
    m1(foragerDead) shouldBe 0
    m1(queenAlive)  shouldBe 1
  }

  // ══════════════════════════════════════════════════════════════════════════
  // 5. P-invariants — vérifications ciblées
  // ══════════════════════════════════════════════════════════════════════════

  "checkPInvariants" should "ne signaler aucune violation sur M₀" in {
    checkPInvariants(initialMarking) shouldBe empty
  }

  it should "détecter PI1 si reine dans 2 états simultanés" in {
    val mBad = Marking(initialMarking.tokens + (queenDead -> 1))
    checkPInvariants(mBad).exists(_.contains("PI1")) shouldBe true
  }

  it should "détecter PI2 si hunger_token > 11" in {
    val mBad = Marking(Map(queenAlive -> 1, hungerToken -> 12))
    checkPInvariants(mBad).exists(_.contains("PI2")) shouldBe true
  }

  it should "détecter PI2 si hunger_token < 0" in {
    // Cas théorique (P/T pur ne produit jamais cela — test de robustesse)
    val mBad = Marking(Map(queenAlive -> 1, hungerToken -> -1))
    checkPInvariants(mBad).exists(_.contains("PI2")) shouldBe true
  }

  it should "détecter PI3 si >4 fourrageuses" in {
    val mBad = Marking(Map(queenAlive -> 1, hungerToken -> 5, foragerIdle -> 5))
    checkPInvariants(mBad).exists(_.contains("PI3")) shouldBe true
  }

  it should "détecter PI4 si >4 transporteuses" in {
    val mBad = Marking(Map(queenAlive -> 1, hungerToken -> 5, carrierIdle -> 5))
    checkPInvariants(mBad).exists(_.contains("PI4")) shouldBe true
  }

  it should "détecter PI5 si stock > 20" in {
    val mBad = Marking(Map(queenAlive -> 1, hungerToken -> 5, stock -> 21))
    checkPInvariants(mBad).exists(_.contains("PI5")) shouldBe true
  }

  it should "détecter PI6 si reine vivante avec faim > 11" in {
    val mBad = Marking(Map(queenAlive -> 1, hungerToken -> 12))
    checkPInvariants(mBad).exists(_.contains("PI6")) shouldBe true
  }

  it should "ne pas signaler PI1 si queen_dead seule" in {
    val mDead = Marking(Map(queenDead -> 1))
    checkPInvariants(mDead) shouldBe empty
  }

  // ══════════════════════════════════════════════════════════════════════════
  // 6. Vérification analytique des P-invariants (y^T · C = 0)
  // ══════════════════════════════════════════════════════════════════════════

  "isPInvariant" should "confirmer que PI1 est un vrai P-invariant" in {
    val yPI1 = Map(queenAlive -> 1, queenDead -> 1, queenSated -> 1)
    isPInvariant(yPI1) shouldBe true
  }

  it should "rejeter un vecteur aléatoire non-invariant" in {
    // Un vecteur qui isole uniquement queen_alive ne peut pas être conservé
    // car t_tick produit queen_alive sans consommer d'autre copie
    val yBad = Map(foragerIdle -> 1)   // pas d'invariant pour forager_idle seul
    isPInvariant(yBad) shouldBe false
  }

  // ══════════════════════════════════════════════════════════════════════════
  // 7. Exploration BFS de l'espace d'états
  // ══════════════════════════════════════════════════════════════════════════

  "reachabilityGraph" should "contenir M₀" in {
    reachable should contain(initialMarking)
  }

  it should "contenir plus d'un marquage" in {
    reachable.size should be > 1
  }

  it should "ne pas dépasser maxStates" in {
    reachable.size should be <= 5000
  }

  it should "produire des arcs dont source et destination sont dans reachable" in {
    edges.foreach { case (src, _, dst) =>
      reachable should contain(src)
      reachable should contain(dst)
    }
  }

  it should "ne jamais produire M(stock) < 0" in {
    reachable.foreach(m => m(stock) should be >= 0)
  }

  it should "ne jamais produire M(hunger_token) < 0" in {
    reachable.foreach(m => m(hungerToken) should be >= 0)
  }

  it should "ne jamais avoir queen_alive = queen_dead = 1 simultanément" in {
    reachable.foreach { m =>
      (m(queenAlive) == 1 && m(queenDead) == 1) shouldBe false
    }
  }

  // ══════════════════════════════════════════════════════════════════════════
  // 8. Deadlocks
  // ══════════════════════════════════════════════════════════════════════════

  "deadlocks" should "ne contenir que des marquages sans transition franchissable" in {
    deadlockMarkings.foreach(m => net.enabled(m) shouldBe empty)
  }

  it should "ne contenir que des états avec queen_dead = 1 (LTL8)" in {
    verifyLTL8(deadlockMarkings) shouldBe true
  }

  // ══════════════════════════════════════════════════════════════════════════
  // 9. Vérification LTL
  // ══════════════════════════════════════════════════════════════════════════

  "verifyLTL1 (état reine)" should "être vrai" in {
    verifyLTL1(reachable) shouldBe true
  }

  "verifyLTL2 (stock ≥ 0)" should "être vrai" in {
    verifyLTL2(reachable) shouldBe true
  }

  "verifyLTL3 (population bornée)" should "être vrai" in {
    verifyLTL3(reachable) shouldBe true
  }

  "verifyLTL4 (faim bornée)" should "être vrai" in {
    verifyLTL4(reachable) shouldBe true
  }

  "verifyLTL5 (vivacité fourrageuse)" should "être vrai" in {
    verifyLTL5(reachable) shouldBe true
  }

  "verifyLTL6 (vivacité transporteuse)" should "être vrai" in {
    verifyLTL6(reachable) shouldBe true
  }

  "verifyLTL7 (vivacité œuf)" should "être vrai" in {
    verifyLTL7(reachable) shouldBe true
  }

  "verifyLTL8 (deadlock → queen_dead)" should "être vrai" in {
    verifyLTL8(deadlockMarkings) shouldBe true
  }

  "verifyLTL9 (terminaison sans fourmis)" should "être vrai" in {
    verifyLTL9(reachable) shouldBe true
  }

  // ══════════════════════════════════════════════════════════════════════════
  // 10. Scénarios de bout en bout
  // ══════════════════════════════════════════════════════════════════════════

  "Le flux nominal fourrageuse" should "suivre idle → resting → resting_waiting → idle" in {
    // SearchFood
    val m1 = initialMarking.fire(tSearchFood)
    m1(foragerIdle) shouldBe 0; m1(foragerResting) shouldBe 1; m1(stock) shouldBe 2

    // AntTick + ConsumeFood (stock ≥ 1)
    m1.enables(tForagerConsume) shouldBe true
    val m2 = m1.fire(tForagerConsume)
    m2(foragerResting) shouldBe 0; m2(foragerRestingWaiting) shouldBe 1; m2(stock) shouldBe 1

    // FoodReady → idle
    m2.enables(tForagerFed) shouldBe true
    val m3 = m2.fire(tForagerFed)
    m3(foragerRestingWaiting) shouldBe 0; m3(foragerIdle) shouldBe 1
  }

  "Le flux nominal transporteuse" should "suivre idle → waiting → resting → idle" in {
    val mStock = Marking(initialMarking.tokens + (stock -> 5))

    // RequestFood
    val m1 = mStock.fire(tRequestFood)
    m1(carrierIdle) shouldBe 0; m1(carrierWaiting) shouldBe 1; m1(stock) shouldBe 3

    // CarrierDeliver (hunger suffisant)
    val m2 = Marking(m1.tokens + (hungerToken -> 6))   // s'assure hunger ≥ 2
    m2.enables(tCarrierDeliver) shouldBe true
    val m3 = m2.fire(tCarrierDeliver)
    m3(carrierWaiting) shouldBe 0; m3(carrierResting) shouldBe 1
    m3(hungerToken) shouldBe 4  // +5 initial + 6 artificiel, mais le marquage m2 avait 6 → 6-2=4... corrigeons
    // Note : m2 hérite de m1 (hunger = 5 de initialMarking), + on ajoute 6 via tokens update
    // Recalcul : m1 n'a pas de hungerToken additionnel, m2 = m1 + {hungerToken → 6}
    // En réalité m1(hungerToken) = 5 (hérité) ; m2 = tokens + hungerToken → 6 (remplace)
    // donc m2(hungerToken) = 6, après deliver = 6-2 = 4
    m3(hungerToken) shouldBe 4

    // AntTick resting + ConsumeFood
    m3.enables(tCarrierConsume) shouldBe true
    val m4 = m3.fire(tCarrierConsume)
    m4(carrierResting) shouldBe 0; m4(carrierRestingWaiting) shouldBe 1

    // FoodReady → idle
    val m5 = m4.fire(tCarrierFed)
    m5(carrierRestingWaiting) shouldBe 0; m5(carrierIdle) shouldBe 1
  }

  "Le cycle de mort de la reine" should "se produire quand hunger_token atteint 11" in {
    val mCrit = Marking(Map(queenAlive -> 1, hungerToken -> 11, foragerIdle -> 1, carrierIdle -> 1))
    mCrit.enables(tQueenDies) shouldBe true

    val mDead = mCrit.fire(tQueenDies)
    mDead(queenAlive) shouldBe 0; mDead(queenDead) shouldBe 1;
  }

  "3 AntTick sans nourriture (fourrageuse)" should "mener à la mort" in {
    val m1 = initialMarking.fire(tForagerStarveIdle)
    m1(foragerStarvation) shouldBe 1
    val m2 = m1.fire(tForagerStarveIdle)
    m2(foragerStarvation) shouldBe 2
    val m3 = m2.fire(tForagerStarveIdle)
    m3(foragerStarvation) shouldBe 3
    m3.enables(tForagerDiesIdle) shouldBe true
    val mDead = m3.fire(tForagerDiesIdle)
    mDead(foragerIdle) shouldBe 0; mDead(foragerDead) shouldBe 1
    mDead(foragerStarvation) shouldBe 0   // consommé
  }

  "3 AntTick sans nourriture (transporteuse)" should "mener à la mort" in {
    val m1 = initialMarking.fire(tCarrierStarveIdle)
    val m2 = m1.fire(tCarrierStarveIdle)
    val m3 = m2.fire(tCarrierStarveIdle)
    m3(carrierStarvation) shouldBe 3
    m3.enables(tCarrierDiesIdle) shouldBe true
    val mDead = m3.fire(tCarrierDiesIdle)
    mDead(carrierIdle) shouldBe 0; mDead(carrierDead) shouldBe 1
  }

  // ══════════════════════════════════════════════════════════════════════════
  // 11. Cycle de vie d'un œuf (modélisation correcte à 2 places)
  // ══════════════════════════════════════════════════════════════════════════

  "Le cycle œuf" should "progresser correctement : egg_cycle1 → egg_cycle2 → fourmi" in {
    // La reine pond 2 œufs
    val m1 = initialMarking.fire(tLayEgg)
    m1(eggCycle1) shouldBe 2
    m1(eggCycle2) shouldBe 0

    // 1er Tick sur l'œuf : cycle1 → cycle2
    m1.enables(tEggIncubate) shouldBe true
    val m2 = m1.fire(tEggIncubate)
    m2(eggCycle1) shouldBe 1   // il reste 1 autre œuf en cycle1
    m2(eggCycle2) shouldBe 1

    // 2e Tick sur le 1er œuf : spawn
    m2.enables(tSpawnForager) shouldBe true
    val m3 = m2.fire(tSpawnForager)
    m3(eggCycle2)   shouldBe 0
    m3(foragerIdle) shouldBe 2   // 1 initiale + 1 née
  }

  it should "ne pas faire de boucle no-op (correction vs version précédente)" in {
    // tEggIncubate doit changer l'état : egg_cycle1 diminue, egg_cycle2 augmente
    val mEgg = Marking(initialMarking.tokens + (eggCycle1 -> 1))
    val m1   = mEgg.fire(tEggIncubate)
    m1(eggCycle1) should be < mEgg(eggCycle1)   // consommé
    m1(eggCycle2) should be > mEgg(eggCycle2)   // produit
  }

  // ══════════════════════════════════════════════════════════════════════════
  // 12. Correspondance Akka ↔ transitions
  // ══════════════════════════════════════════════════════════════════════════

  "t_tick" should "correspondre à Tick → hunger+1 dans QueenActor" in {
    val m = initialMarking.fire(tTick)
    m(hungerToken) shouldBe initialMarking(hungerToken) + 1
  }

  "t_carrier_deliver" should "correspondre à FeedQueen(2) → hunger -= 2 dans QueenActor" in {
    val m = Marking(Map(queenAlive -> 1, hungerToken -> 8, carrierWaiting -> 1))
    val m1 = m.fire(tCarrierDeliver)
    m1(hungerToken) shouldBe 6   // 8 - 2
  }

  "t_search_food" should "correspondre à DepositFood dans StorageActor (+2 stock)" in {
    val m = initialMarking.fire(tSearchFood)
    m(stock) shouldBe initialMarking(stock) + 2
  }

  "t_lay_egg" should "correspondre à EggsPerCycle=2 dans QueenActor" in {
    val m = initialMarking.fire(tLayEgg)
    m(eggCycle1) shouldBe 2
  }

  "t_forager_died_notify" should "correspondre à AntDied(Forager) → QueenActor sans tuer la reine" in {
    val m = Marking(Map(queenAlive -> 1, hungerToken -> 5, foragerDead -> 1, carrierIdle -> 1))
    val m1 = m.fire(tForagerDiedNotify)
    m1(foragerDead) shouldBe 0
    m1(queenAlive)  shouldBe 1
  }

  "t_carrier_died_notify" should "correspondre à AntDied(Carrier) → QueenActor sans tuer la reine" in {
    val m = Marking(Map(queenAlive -> 1, hungerToken -> 5, carrierDead -> 1, foragerIdle -> 1))
    val m1 = m.fire(tCarrierDiedNotify)
    m1(carrierDead) shouldBe 0
    m1(queenAlive)  shouldBe 1
  }

  "t_queen_unsated" should "correspondre à hunger=0 → reine reprend Ticks" in {
    val mSated = Marking(Map(queenSated -> 1))
    mSated.enables(tQueenUnsated) shouldBe true
    val m1 = mSated.fire(tQueenUnsated)
    m1(queenSated) shouldBe 0
    m1(queenAlive) shouldBe 1
  }

  "t_forager_starve_resting" should "correspondre à StorageEmpty → famine +1 (restingWaiting)" in {
    val m = Marking(Map(queenAlive -> 1, hungerToken -> 5, foragerRestingWaiting -> 1, carrierIdle -> 1))
    m.enables(tForagerStarveResting) shouldBe true
    val m1 = m.fire(tForagerStarveResting)
    m1(foragerRestingWaiting) shouldBe 0
    m1(foragerResting)        shouldBe 1
    m1(foragerStarvation)     shouldBe 1
  }

  "t_carrier_starve_resting" should "correspondre à StorageEmpty → famine +1 (carrierRestingWaiting)" in {
    val m = Marking(Map(queenAlive -> 1, hungerToken -> 5, carrierRestingWaiting -> 1, foragerIdle -> 1))
    m.enables(tCarrierStarveResting) shouldBe true
    val m1 = m.fire(tCarrierStarveResting)
    m1(carrierRestingWaiting) shouldBe 0
    m1(carrierResting)        shouldBe 1
    m1(carrierStarvation)     shouldBe 1
  }
}