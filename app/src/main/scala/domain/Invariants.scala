package domain

object Invariants {

  val MaxHunger: Int = 10

  // Invariant 1 : la faim ne peut pas être négative
  def hungerIsNonNegative(hunger: Int): Boolean =
    hunger >= 0

  // Invariant 2 : la faim ne peut pas dépasser le maximum
  def hungerBelowMax(hunger: Int): Boolean =
    hunger <= MaxHunger

  // Invariant 3 : la reine est en vie si et seulement si sa faim est dans les bornes
  def queenIsAlive(hunger: Int): Boolean =
    hungerIsNonNegative(hunger) && hungerBelowMax(hunger)

  // Vérifie tous les invariants et retourne la liste de ceux qui sont violés
  def check(hunger: Int): List[String] =
    List(
      Option.when(!hungerIsNonNegative(hunger))("VIOLATION : faim négative impossible"),
      Option.when(!hungerBelowMax(hunger))(s"VIOLATION : faim $hunger dépasse le maximum $MaxHunger"),
    ).flatten
}