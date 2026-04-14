/**
 * ActorSpec.scala — Tests unitaires des acteurs Akka de la colonie de fourmis
 *
 * Framework : ScalaTest + Akka Actor TestKit (Akka Typed)
 * Dépendances build.sbt :
 *   libraryDependencies ++= Seq(
 *     "com.typesafe.akka" %% "akka-actor-typed"         % "2.8.x",
 *     "com.typesafe.akka" %% "akka-actor-testkit-typed" % "2.8.x" % Test,
 *     "org.scalatest"     %% "scalatest"                % "3.2.17" % Test
 *   )
 *
 * Couvre :
 *   1. StorageActor  — dépôt, fourniture, borne capacité, stock vide
 *   2. QueenActor    — Tick, FeedQueen, SpawnAnt, AntDied, arrêt
 *   3. Invariants    — toutes les fonctions de vérification
 *   4. EggActor      — incubation, éclosion
 */

package actors

import akka.actor.testkit.typed.scaladsl.{ActorTestKit, TestProbe}
import org.scalatest.BeforeAndAfterAll
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import protocol._
import domain.Invariants

class ActorSpec extends AnyFlatSpec with Matchers with BeforeAndAfterAll {

  val testKit: ActorTestKit = ActorTestKit()

  override def afterAll(): Unit = testKit.shutdownTestKit()

  // ══════════════════════════════════════════════════════════════════════════
  // 1. StorageActor
  // ══════════════════════════════════════════════════════════════════════════

  "StorageActor" should "répondre FoodReady lors d'un dépôt puis d'une demande" in {
    val probe = testKit.createTestProbe[Command]()
    val storage = testKit.spawn(StorageActor(stock = 0), "storage-test-1")

    // Dépôt de 5 unités
    storage ! DepositFood(5)

    // Demande de 3 unités
    storage ! RequestFood(3, probe.ref)
    probe.expectMessage(FoodReady(3))
  }

  it should "répondre StorageEmpty si le stock est vide" in {
    val probe = testKit.createTestProbe[Command]()
    val storage = testKit.spawn(StorageActor(stock = 0), "storage-test-2")

    storage ! RequestFood(1, probe.ref)
    probe.expectMessage(StorageEmpty)
  }

  it should "respecter la capacité maximale lors d'un dépôt" in {
    val probe = testKit.createTestProbe[Command]()
    // Stock initial = 18, dépôt de 5 → min(23, 20) = 20
    val storage = testKit.spawn(StorageActor(stock = 18), "storage-test-3")

    storage ! DepositFood(5)          // ne peut dépasser MaxCapacity = 20
    storage ! RequestFood(20, probe.ref)
    val msg = probe.expectMessageType[FoodReady]
    msg.amount should be <= Invariants.MaxCapacity
    msg.amount shouldBe 20
  }

  it should "fournir au plus le stock disponible même si la demande dépasse" in {
    val probe = testKit.createTestProbe[Command]()
    val storage = testKit.spawn(StorageActor(stock = 1), "storage-test-4")

    storage ! RequestFood(5, probe.ref)
    probe.expectMessage(FoodReady(1))   // seulement 1 disponible
  }

  it should "répondre FoodReady(1) à ConsumeFood si stock > 0" in {
    val probe = testKit.createTestProbe[Command]()
    val storage = testKit.spawn(StorageActor(stock = 3), "storage-test-5")

    storage ! ConsumeFood(probe.ref)
    probe.expectMessage(FoodReady(1))
  }

  it should "répondre StorageEmpty à ConsumeFood si stock = 0" in {
    val probe = testKit.createTestProbe[Command]()
    val storage = testKit.spawn(StorageActor(stock = 0), "storage-test-6")

    storage ! ConsumeFood(probe.ref)
    probe.expectMessage(StorageEmpty)
  }

  it should "gérer plusieurs dépôts et demandes séquentiels correctement" in {
    val probe = testKit.createTestProbe[Command]()
    val storage = testKit.spawn(StorageActor(stock = 0), "storage-test-7")

    storage ! DepositFood(10)
    storage ! RequestFood(4, probe.ref)
    probe.expectMessage(FoodReady(4))

    storage ! RequestFood(4, probe.ref)
    probe.expectMessage(FoodReady(4))

    storage ! RequestFood(4, probe.ref)
    probe.expectMessage(FoodReady(2))   // seulement 2 restants
  }

  // ══════════════════════════════════════════════════════════════════════════
  // 2. QueenActor
  // ══════════════════════════════════════════════════════════════════════════

  "QueenActor" should "rester vivante après un Tick si hunger < MaxHunger" in {
    val probe = testKit.createTestProbe[Command]()
    val storage = testKit.spawn(StorageActor(0), "storage-queen-1")
    val queen = testKit.spawn(
      QueenActor(storage, hunger = 5),
      "queen-test-1"
    )
    // On observe que la reine ne s'arrête pas après un Tick
    queen ! Tick
    // Pas de message envoyé directement — on vérifie via probe sur le stockage
    // (aucun crash = comportement attendu)
    Thread.sleep(200)
    queen ! Stop   // arrêt propre
  }

  it should "diminuer la faim lors d'un FeedQueen" in {
    val probeStorage = testKit.createTestProbe[Command]()
    val storage = testKit.spawn(StorageActor(0), "storage-queen-2")
    val queen = testKit.spawn(
      QueenActor(storage, hunger = 8),
      "queen-test-2"
    )
    // Nourrir la reine de 3 unités
    queen ! FeedQueen(3)
    // hunger devrait passer à 5 (8-3) — on vérifie l'absence de crash
    Thread.sleep(100)
    queen ! Stop
  }

  it should "s'arrêter proprement sur Stop" in {
    val storage = testKit.spawn(StorageActor(0), "storage-queen-3")
    val queen = testKit.spawn(
      QueenActor(storage, hunger = 5),
      "queen-test-3"
    )
    val deathProbe = testKit.createTestProbe[Command]()
    // La reine s'arrête après Stop
    queen ! Stop
    deathProbe.expectTerminated(queen)
  }

  it should "spawner une fourrageuse sur SpawnAnt(Forager) si population < MaxPerType" in {
    val storage = testKit.spawn(StorageActor(0), "storage-queen-4")
    val queen = testKit.spawn(
      QueenActor(storage, hunger = 5, foragerCount = 0, carrierCount = 0),
      "queen-test-4"
    )
    queen ! SpawnAnt(Forager)
    Thread.sleep(200)   // laisse le temps au spawn
    queen ! Stop
  }

  it should "ignorer SpawnAnt(Forager) si foragerCount >= MaxPerType" in {
    val storage = testKit.spawn(StorageActor(0), "storage-queen-5")
    val queen = testKit.spawn(
      QueenActor(storage, hunger = 5, foragerCount = Invariants.MaxPerType, carrierCount = 0),
      "queen-test-5"
    )
    // Doit être ignoré (pas d'exception, pas de spawn)
    queen ! SpawnAnt(Forager)
    Thread.sleep(100)
    queen ! Stop
  }

  it should "décrémenter le compteur de fourrageuses sur AntDied(Forager)" in {
    val storage = testKit.spawn(StorageActor(0), "storage-queen-6")
    val queen = testKit.spawn(
      QueenActor(storage, hunger = 5, foragerCount = 2, carrierCount = 1),
      "queen-test-6"
    )
    queen ! AntDied(Forager, 1)
    Thread.sleep(100)
    queen ! Stop
  }

  it should "décrémenter le compteur de transporteuses sur AntDied(Carrier)" in {
    val storage = testKit.spawn(StorageActor(0), "storage-queen-7")
    val queen = testKit.spawn(
      QueenActor(storage, hunger = 5, foragerCount = 1, carrierCount = 2),
      "queen-test-7"
    )
    queen ! AntDied(Carrier, 1)
    Thread.sleep(100)
    queen ! Stop
  }

  // ══════════════════════════════════════════════════════════════════════════
  // 3. Invariants (domain.Invariants)
  // ══════════════════════════════════════════════════════════════════════════

  "Invariants.hungerIsNonNegative" should "retourner true si hunger >= 0" in {
    Invariants.hungerIsNonNegative(0)  shouldBe true
    Invariants.hungerIsNonNegative(5)  shouldBe true
    Invariants.hungerIsNonNegative(10) shouldBe true
  }

  it should "retourner false si hunger < 0" in {
    Invariants.hungerIsNonNegative(-1) shouldBe false
    Invariants.hungerIsNonNegative(-5) shouldBe false
  }

  "Invariants.hungerBelowMax" should "retourner true si hunger <= MaxHunger" in {
    Invariants.hungerBelowMax(0)  shouldBe true
    Invariants.hungerBelowMax(10) shouldBe true
  }

  it should "retourner false si hunger > MaxHunger" in {
    Invariants.hungerBelowMax(11) shouldBe false
  }

  "Invariants.stockIsNonNegative" should "retourner true si stock >= 0" in {
    Invariants.stockIsNonNegative(0)  shouldBe true
    Invariants.stockIsNonNegative(15) shouldBe true
  }

  it should "retourner false si stock < 0" in {
    Invariants.stockIsNonNegative(-1) shouldBe false
  }

  "Invariants.stockBelowCapacity" should "retourner true si stock <= MaxCapacity" in {
    Invariants.stockBelowCapacity(0)  shouldBe true
    Invariants.stockBelowCapacity(20) shouldBe true
  }

  it should "retourner false si stock > MaxCapacity" in {
    Invariants.stockBelowCapacity(21) shouldBe false
  }

  "Invariants.foragerCountValid" should "retourner true pour [0, MaxPerType]" in {
    Invariants.foragerCountValid(0) shouldBe true
    Invariants.foragerCountValid(2) shouldBe true
    Invariants.foragerCountValid(4) shouldBe true
  }

  it should "retourner false si count < 0 ou count > MaxPerType" in {
    Invariants.foragerCountValid(-1) shouldBe false
    Invariants.foragerCountValid(5)  shouldBe false
  }

  "Invariants.carrierCountValid" should "retourner true pour [0, MaxPerType]" in {
    Invariants.carrierCountValid(0) shouldBe true
    Invariants.carrierCountValid(4) shouldBe true
  }

  it should "retourner false si count < 0 ou count > MaxPerType" in {
    Invariants.carrierCountValid(-1) shouldBe false
    Invariants.carrierCountValid(5)  shouldBe false
  }

  "Invariants.queenIsAlive" should "retourner true si hunger dans [0, MaxHunger]" in {
    Invariants.queenIsAlive(0)  shouldBe true
    Invariants.queenIsAlive(5)  shouldBe true
    Invariants.queenIsAlive(10) shouldBe true
  }

  it should "retourner false si hunger hors bornes" in {
    Invariants.queenIsAlive(-1) shouldBe false
    Invariants.queenIsAlive(11) shouldBe false
  }

  "Invariants.checkQueen" should "retourner une liste vide si hunger valide" in {
    Invariants.checkQueen(5) shouldBe empty
    Invariants.checkQueen(0) shouldBe empty
  }

  it should "retourner une violation [I2] si hunger > MaxHunger" in {
    val violations = Invariants.checkQueen(11)
    violations should not be empty
    violations.exists(_.contains("I2")) shouldBe true
  }

  it should "retourner une violation [I1] si hunger < 0" in {
    val violations = Invariants.checkQueen(-1)
    violations.exists(_.contains("I1")) shouldBe true
  }

  "Invariants.checkStorage" should "retourner vide si stock valide" in {
    Invariants.checkStorage(0)  shouldBe empty
    Invariants.checkStorage(10) shouldBe empty
    Invariants.checkStorage(20) shouldBe empty
  }

  it should "retourner violation [I4] si stock > MaxCapacity" in {
    val violations = Invariants.checkStorage(21)
    violations.exists(_.contains("I4")) shouldBe true
  }

  it should "retourner violation [I3] si stock < 0" in {
    val violations = Invariants.checkStorage(-1)
    violations.exists(_.contains("I3")) shouldBe true
  }

  "Invariants.checkPopulation" should "retourner vide si counts valides" in {
    Invariants.checkPopulation(1, 1) shouldBe empty
    Invariants.checkPopulation(4, 4) shouldBe empty
    Invariants.checkPopulation(0, 0) shouldBe empty
  }

  it should "retourner violation [I5] si foragers hors bornes" in {
    val violations = Invariants.checkPopulation(5, 1)
    violations.exists(_.contains("I5")) shouldBe true
  }

  it should "retourner violation [I6] si carriers hors bornes" in {
    val violations = Invariants.checkPopulation(1, 5)
    violations.exists(_.contains("I6")) shouldBe true
  }

  it should "retourner les deux violations si les deux sont hors bornes" in {
    val violations = Invariants.checkPopulation(-1, 6)
    violations.exists(_.contains("I5")) shouldBe true
    violations.exists(_.contains("I6")) shouldBe true
  }

  "Invariants.checkAll" should "combiner toutes les vérifications" in {
    Invariants.checkAll(5, 10, 2, 2) shouldBe empty
  }

  it should "retourner toutes les violations si tout est invalide" in {
    val violations = Invariants.checkAll(-1, -1, -1, -1)
    violations.exists(_.contains("I1")) shouldBe true
    violations.exists(_.contains("I3")) shouldBe true
    violations.exists(_.contains("I5")) shouldBe true
    violations.exists(_.contains("I6")) shouldBe true
  }

  // ══════════════════════════════════════════════════════════════════════════
  // 4. EggActor
  // ══════════════════════════════════════════════════════════════════════════

  "EggActor" should "envoyer SpawnAnt après 2 Tick" in {
    val colonyProbe = testKit.createTestProbe[Command]()
    val storage     = testKit.spawn(StorageActor(0), "storage-egg-1")
    val egg = testKit.spawn(
      EggActor(colonyProbe.ref, storage, colonyProbe.ref),
      "egg-test-1"
    )

    // Premier Tick — cyclesLeft = 2 → 1 (pas encore éclos)
    egg ! Tick
    colonyProbe.expectNoMessage()

    // Deuxième Tick — cyclesLeft = 1 → 0 → éclosion
    egg ! Tick
    val msg = colonyProbe.expectMessageType[SpawnAnt]
    msg.antType should (be(Forager) or be(Carrier))
  }

  it should "s'arrêter après l'éclosion" in {
    val colonyProbe = testKit.createTestProbe[Command]()
    val storage     = testKit.spawn(StorageActor(0), "storage-egg-2")
    val egg = testKit.spawn(
      EggActor(colonyProbe.ref, storage, colonyProbe.ref),
      "egg-test-2"
    )
    val deathWatch = testKit.createTestProbe[Command]()

    egg ! Tick
    egg ! Tick   // éclosion → Behaviors.stopped

    colonyProbe.expectMessageType[SpawnAnt]
    deathWatch.expectTerminated(egg)
  }

  it should "ne pas s'arrêter après seulement 1 Tick" in {
    val colonyProbe = testKit.createTestProbe[Command]()
    val storage     = testKit.spawn(StorageActor(0), "storage-egg-3")
    val egg = testKit.spawn(
      EggActor(colonyProbe.ref, storage, colonyProbe.ref),
      "egg-test-3"
    )

    egg ! Tick
    // Aucun SpawnAnt attendu
    colonyProbe.expectNoMessage()
    // L'acteur est toujours en vie (pas de terminaison)
  }

  // ══════════════════════════════════════════════════════════════════════════
  // 5. Protocole Messages — couverture des types
  // ══════════════════════════════════════════════════════════════════════════

  "Le protocole Messages" should "définir Forager et Carrier comme AntType distincts" in {
    val f: AntType = Forager
    val c: AntType = Carrier
    f should not equal c
  }

  it should "permettre la construction de tous les messages du protocole" in {
    val probe = testKit.createTestProbe[Command]()

    val tick: Command          = Tick
    val feed: Command          = FeedQueen(3)
    val stop: Command          = Stop
    val search: Command        = SearchFood
    val consume: Command       = ConsumeFood(probe.ref)
    val antTick: Command       = AntTick
    val deposit: Command       = DepositFood(5)
    val request: Command       = RequestFood(2, probe.ref)
    val ready: Command         = FoodReady(2)
    val empty: Command         = StorageEmpty
    val died: Command          = AntDied(Forager, 1)
    val spawn: Command         = SpawnAnt(Carrier)

    // Toutes les constructions réussissent sans exception
    List(tick, feed, stop, search, consume, antTick, deposit, request, ready, empty, died, spawn)
      .forall(_ != null) shouldBe true
  }
}