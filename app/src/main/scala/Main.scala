package main

import akka.actor.typed.ActorSystem
import simulation.Simulation

object Main {
  def main(args: Array[String]): Unit = {
    val system = ActorSystem[Nothing](Simulation(), "AntColonySystem")
    // Garde le programme vivant
    //Thread.sleep(Long.MaxValue)

    println(">>> Simulation lancée. Appuie sur ENTREE pour arrêter. <<<")

    scala.io.StdIn.readLine() // Le programme attend que tu appuies sur Entrée

    system.terminate() // Arrête proprement le système d'acteurs
  }
}