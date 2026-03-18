//Definition d'un noeud/sommet
sealed trait Node {
  val numero: String
}

//Definition du stockage
case class Place(
    numero: String,
    var emplacementLibre: Int) extends Node

//Definition de verification de transition 
case class Transition(
  numero: String,
  condition: Map[String, Int] => Boolean) extends Node

//Definition d'une arête 
case class Edge(
  from: Node,
  to: Node,
  weight: Int 
)

//Definition d'un graphe
class PetriNet(
  val places: Map[String, Place],
  val transitions: Map[String, Transition],
  val edges: List[Edge]
) {

  //definition du marquage
  def marking(): Map[String, Int] = {
    places.map { case (numero, place) => numero -> place.emplacementLibre }
  }

  //Definition des conditions de transition
  def isEnabled(t: Transition): Boolean = {

    val inputEdges = edges.filter(e => e.to == t)

    val tokensOk = inputEdges.forall {
      case Edge(from: Place, _, weight) =>
        from.emplacementLibre >= weight
      case _ => true
    }

    tokensOk && t.condition(marking())
  }

  //Definition de la transition
  def fire(t: Transition): Unit = {

    if (!isEnabled(t)) {
      println(s"Transition ${t.numero} non activée")
      return
    }

    val inputEdges = edges.filter(_.to == t)
    val outputEdges = edges.filter(_.from == t)

    //Ici on déplace les poids d'un sommet vers une transition ou l'inverse
    inputEdges.foreach {
      case Edge(from: Place, _, weight) =>
        from.emplacementLibre -= weight
      case _ =>
    }

    outputEdges.foreach {
      case Edge(_, to: Place, weight) =>
        to.emplacementLibre += weight
      case _ =>
    }

    println(s"Transition ${t.numero} exécutée")
  }

  //définition d'une simulation automatique
  def simulate(etape: Int): Unit = {

  for (i <- 1 to etape) {

    println(s"\nétape $i")
    println("Marqué: " + marking())

    val enabled = transitions.values.filter(isEnabled).toList
    //cas blocage
    if (enabled.isEmpty) {
      println("Aucune transition activée")
    }

    val t = enabled.head

    fire(t)
  }

}

  object PetriNetSimulation extends App {

    val p1 = Place("P1", 1)
    val p2 = Place("P2", 0)

    val t1 = Transition("T1", _ => true)

    val adjacency: Map[Node, List[Edge]] = Map(
      p1 -> List(Edge(t1,p2, 1)),
      t1 -> List(Edge(p2,t1, 1)),
      p2 -> List()
    )

    val net = new PetriNet(
      Map("P1" -> p1, "P2" -> p2),
      Map("T1" -> t1),
      edges
    )

    net.simulate(5)

  }

}












