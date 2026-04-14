package domain

/**
* Invariants définis:
  * I1—La faim de la reine est toujours >= 0
  * I2—La faim de la reine est toujours <= MaxHunger
  * I3—Le stock est toujours >= 0
  * I4—Le stock est toujours <= MaxCapacity
  * I5—Le nombre de fourrageuses est toujours dans[0, MaxPerType]
  * I6—Le nombre de transporteuses est toujours dans[0, MaxPerType]
  * I7—La reine est vivante ssi sa faim est strictement < MaxHunger
*/

object Invariants {

  val MaxHunger: Int = 10
  val MaxCapacity: Int = 20
  val MaxPerType: Int = 4

  // Invariant 1 : la faim ne peut pas être négative
  def hungerIsNonNegative(hunger: Int): Boolean =
    hunger >= 0

  // Invariant 2 : la faim ne peut pas dépasser le maximum
  def hungerBelowMax(hunger: Int): Boolean =
    hunger <= MaxHunger

  // Invariant 3 : le stock ne peut jamais être négatif
  def stockIsNonNegative(stock: Int): Boolean = stock >= 0

  // Invariant 4  : le stock ne peut pas dépasser la capacité maximale
  def stockBelowCapacity(stock: Int): Boolean = stock <= MaxCapacity

  // Invariant 5  : le nombre de fourrageuses reste dans les bornes
  def foragerCountValid(count: Int): Boolean = count >= 0 && count <= MaxPerType

  // Invariant 6  : le nombre de transporteuses reste dans les bornes
  def carrierCountValid(count: Int): Boolean = count >= 0 && count <= MaxPerType

  // Invariant 7 : la reine est en vie si et seulement si sa faim est dans les bornes
  def queenIsAlive(hunger: Int): Boolean =
    hungerIsNonNegative(hunger) && hungerBelowMax(hunger)

  // ── Vérification groupée ───────────────────────────────────────────────────

  /**
   * Vérifie les invariants portant sur la reine (faim).
   * Appelé à chaque Tick dans QueenActor.
   */
  def checkQueen(hunger: Int): List[String] =
    List(
      Option.when(!hungerIsNonNegative(hunger))(
        s"[I1] VIOLATION : faim négative ($hunger < 0) — impossible"),
      Option.when(!hungerBelowMax(hunger))(
        s"[I2] VIOLATION : faim $hunger dépasse le maximum $MaxHunger"),
    ).flatten

  /**
   * Vérifie les invariants portant sur le stockage.
   * Appelé à chaque modification dans StorageActor.
   */
  def checkStorage(stock: Int): List[String] =
    List(
      Option.when(!stockIsNonNegative(stock))(
        s"[I3] VIOLATION : stock négatif ($stock < 0) — impossible"),
      Option.when(!stockBelowCapacity(stock))(
        s"[I4] VIOLATION : stock $stock dépasse la capacité $MaxCapacity"),
    ).flatten

  /**
   * Vérifie les invariants portant sur la population de fourmis.
   * Appelé dans QueenActor à chaque SpawnAnt / AntDied.
   */
  def checkPopulation(foragers: Int, carriers: Int): List[String] =
    List(
      Option.when(!foragerCountValid(foragers))(
        s"[I5] VIOLATION : nombre de fourrageuses invalide ($foragers) — attendu [0, $MaxPerType]"),
      Option.when(!carrierCountValid(carriers))(
        s"[I6] VIOLATION : nombre de transporteuses invalide ($carriers) — attendu [0, $MaxPerType]"),
    ).flatten

  /**
   * Vérifie tous les invariants d'un seul coup (pour les tests ou le rapport).
   */
  def checkAll(hunger: Int, stock: Int, foragers: Int, carriers: Int): List[String] =
    checkQueen(hunger) ++ checkStorage(stock) ++ checkPopulation(foragers, carriers)
}