package main

import akka.actor.typed.ActorSystem
import simulation.{Simulation, SimulationLogger}

object Main {
  def main(args: Array[String]): Unit = {
    val system = ActorSystem[Nothing](Simulation(), "AntColonySystem")
    // Garde le programme vivant
    //Thread.sleep(Long.MaxValue)

    println(">>> Simulation lancée. Appuie sur ENTREE pour arrêter. <<<")
    println(s">>> Les events sont loggés dans simulation_log.jsonl <<<")
    scala.io.StdIn.readLine() // Le programme attend que tu appuies sur Entrée

    // Ferme proprement le logger avant d'arrêter le système
    SimulationLogger.close()
    system.terminate() // Arrête proprement le système d'acteurs

    println(">>> Simulation arrêtée proprement. <<<")
  }
}