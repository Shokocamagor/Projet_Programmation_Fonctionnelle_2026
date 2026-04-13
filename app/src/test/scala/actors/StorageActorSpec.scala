package actors

import akka.actor.testkit.typed.scaladsl.ScalaTestWithActorTestKit
import org.scalatest.wordspec.AnyWordSpecLike
import protocol._
import domain.Invariants

class StorageActorSpec extends ScalaTestWithActorTestKit with AnyWordSpecLike {

  "StorageActor" when {

    "stock est vide" should {
      "répondre StorageEmpty à un RequestFood" in {
        val storage = spawn(StorageActor(stock = 0))
        val probe = createTestProbe[Command]()
        storage ! RequestFood(2, probe.ref)
        probe.expectMessage(StorageEmpty)
      }
    }

    "stock est suffisant" should {
      "fournir exactement le montant demandé" in {
        val storage = spawn(StorageActor(stock = 5))
        val probe = createTestProbe[Command]()
        storage ! RequestFood(2, probe.ref)
        probe.expectMessage(FoodReady(2))
      }

      "fournir ce qui reste si stock < demande" in {
        val storage = spawn(StorageActor(stock = 1))
        val probe = createTestProbe[Command]()
        storage ! RequestFood(5, probe.ref)
        probe.expectMessage(FoodReady(1))
      }
    }

    "on dépose de la nourriture" should {
      "plafonner le stock à MaxCapacity" in {
        // On utilise la valeur des Invariants pour être cohérent
        val storage = spawn(StorageActor(stock = Invariants.MaxCapacity - 1))
        val probe = createTestProbe[Command]()

        storage ! DepositFood(10)
        storage ! RequestFood(Invariants.MaxCapacity + 10, probe.ref)
        probe.expectMessage(FoodReady(Invariants.MaxCapacity))
      }
    }
  }
}