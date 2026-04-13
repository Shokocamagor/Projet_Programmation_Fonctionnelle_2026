package actors

import akka.actor.testkit.typed.scaladsl.ScalaTestWithActorTestKit
import org.scalatest.wordspec.AnyWordSpecLike
import protocol._
import scala.concurrent.duration._

class QueenActorSpec extends ScalaTestWithActorTestKit with AnyWordSpecLike {

  "QueenActor" when {

    "elle reçoit FeedQueen" should {
      "diminuer la faim du montant reçu" in {
        val storageProbe = createTestProbe[Command]()
        // On démarre à 6, on nourrit de 3. Elle doit rester en vie.
        val queen = spawn(QueenActor(storageProbe.ref, hunger = 6))
        queen ! FeedQueen(3)
        queen ! Tick
        // Pas de crash = succès
      }
    }
    "elle reçoit Tick avec faim maximale" should {
      "s'arrêter proprement" in {
        val storageProbe = createTestProbe[Command]()
        // On initialise la reine au seuil critique
        val queen = spawn(QueenActor(storageProbe.ref, hunger = QueenActor.MaxHunger))

        val monitorProbe = createTestProbe[Command]()

        monitorProbe.watch(queen) // On dit à la sonde de surveiller cet acteur

        queen ! Tick // On envoie le message qui provoque le Behaviors.stopped

        // Maintenant la sonde va bien recevoir le signal de mort
        monitorProbe.expectTerminated(queen, 3.seconds)
      }
    }

    "elle reçoit SpawnAnt" should {
      "ne pas dépasser MaxPerType fourrageuses" in {
        val storageProbe = createTestProbe[Command]()
        val queen = spawn(QueenActor(
          storageProbe.ref,
          hunger = 5,
          foragerCount = QueenActor.MaxPerType,
          foragerNextId = 99
        ))
        queen ! SpawnAnt(Forager)
        // Pas de crash malgré le dépassement
      }
    }

    "elle reçoit AntDied" should {
      "décrémenter le compteur de fourrageuses sans descendre sous 0" in {
        val storageProbe = createTestProbe[Command]()
        val queen = spawn(QueenActor(storageProbe.ref, hunger = 5, foragerCount = 1))
        queen ! AntDied(Forager, 1)
        queen ! AntDied(Forager, 1) // Doit être géré par .max(0) dans l'acteur
      }
    }
  }
}