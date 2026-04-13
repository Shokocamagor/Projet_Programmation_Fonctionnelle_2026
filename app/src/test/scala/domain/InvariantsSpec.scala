// app/src/test/scala/domain/InvariantsSpec.scala
package domain

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class InvariantsSpec extends AnyFlatSpec with Matchers {

  // ── I1 : faim non négative ─────────────────────────────────────────────────
  "hungerIsNonNegative" should "accepter une faim de 0" in {
    Invariants.hungerIsNonNegative(0) shouldBe true
  }
  it should "accepter une faim positive" in {
    Invariants.hungerIsNonNegative(5) shouldBe true
  }
  it should "rejeter une faim négative" in {
    Invariants.hungerIsNonNegative(-1) shouldBe false
  }

  // ── I2 : faim sous le maximum ──────────────────────────────────────────────
  "hungerBelowMax" should "accepter une faim à 0" in {
    Invariants.hungerBelowMax(0) shouldBe true
  }
  it should "accepter une faim égale au max" in {
    Invariants.hungerBelowMax(Invariants.MaxHunger) shouldBe true
  }
  it should "rejeter une faim dépassant le max" in {
    Invariants.hungerBelowMax(Invariants.MaxHunger + 1) shouldBe false
  }

  // ── I3 : stock non négatif ─────────────────────────────────────────────────
  "stockIsNonNegative" should "accepter un stock de 0" in {
    Invariants.stockIsNonNegative(0) shouldBe true
  }
  it should "rejeter un stock négatif" in {
    Invariants.stockIsNonNegative(-1) shouldBe false
  }

  // ── I4 : stock sous la capacité ───────────────────────────────────────────
  "stockBelowCapacity" should "accepter un stock égal à la capacité max" in {
    Invariants.stockBelowCapacity(Invariants.MaxCapacity) shouldBe true
  }
  it should "rejeter un stock dépassant la capacité" in {
    Invariants.stockBelowCapacity(Invariants.MaxCapacity + 1) shouldBe false
  }

  // ── I5 : population fourrageuses ──────────────────────────────────────────
  "foragerCountValid" should "accepter 0 fourrageuse" in {
    Invariants.foragerCountValid(0) shouldBe true
  }
  it should "accepter le maximum" in {
    Invariants.foragerCountValid(Invariants.MaxPerType) shouldBe true
  }
  it should "rejeter un dépassement du maximum" in {
    Invariants.foragerCountValid(Invariants.MaxPerType + 1) shouldBe false
  }
  it should "rejeter un nombre négatif" in {
    Invariants.foragerCountValid(-1) shouldBe false
  }

  // ── I6 : population transporteuses ────────────────────────────────────────
  "carrierCountValid" should "accepter 0 transporteuse" in {
    Invariants.carrierCountValid(0) shouldBe true
  }
  it should "rejeter un dépassement du maximum" in {
    Invariants.carrierCountValid(Invariants.MaxPerType + 1) shouldBe false
  }

  // ── I7 : cohérence vie/mort ────────────────────────────────────────────────
  "queenIsAlive" should "considérer la reine vivante à faim = 0" in {
    Invariants.queenIsAlive(0) shouldBe true
  }
  it should "considérer la reine vivante à faim = MaxHunger - 1" in {
    Invariants.queenIsAlive(Invariants.MaxHunger - 1) shouldBe true
  }
  it should "considérer la reine morte à faim = MaxHunger" in {
    Invariants.queenIsAlive(Invariants.MaxHunger) shouldBe false
  }

  // ── checkQueen ─────────────────────────────────────────────────────────────
  "checkQueen" should "retourner une liste vide si tous les invariants sont respectés" in {
    Invariants.checkQueen(5) shouldBe empty
  }
  it should "détecter une faim dépassant le max" in {
    val violations = Invariants.checkQueen(Invariants.MaxHunger + 1)
    violations should not be empty
    violations.exists(_.contains("[I2]")) shouldBe true
  }

  // ── checkStorage ──────────────────────────────────────────────────────────
  "checkStorage" should "retourner une liste vide pour un stock valide" in {
    Invariants.checkStorage(10) shouldBe empty
  }
  it should "détecter un stock négatif" in {
    val violations = Invariants.checkStorage(-1)
    violations.exists(_.contains("[I3]")) shouldBe true
  }
  it should "détecter un stock trop élevé" in {
    val violations = Invariants.checkStorage(Invariants.MaxCapacity + 1)
    violations.exists(_.contains("[I4]")) shouldBe true
  }

  // ── checkPopulation ────────────────────────────────────────────────────────
  "checkPopulation" should "retourner une liste vide pour des counts valides" in {
    Invariants.checkPopulation(2, 3) shouldBe empty
  }
  it should "détecter un dépassement du nombre de fourrageuses" in {
    val violations = Invariants.checkPopulation(Invariants.MaxPerType + 1, 1)
    violations.exists(_.contains("[I5]")) shouldBe true
  }
  it should "détecter un dépassement du nombre de transporteuses" in {
    val violations = Invariants.checkPopulation(1, Invariants.MaxPerType + 1)
    violations.exists(_.contains("[I6]")) shouldBe true
  }

  // ── checkAll ──────────────────────────────────────────────────────────────
  "checkAll" should "retourner toutes les violations simultanément" in {
    // Stock négatif + trop de fourrageuses
    val violations = Invariants.checkAll(
      hunger   = 5,
      stock    = -1,
      foragers = Invariants.MaxPerType + 1,
      carriers = 0
    )
    violations.size shouldBe 2
    violations.exists(_.contains("[I3]")) shouldBe true
    violations.exists(_.contains("[I5]")) shouldBe true
  }
  it should "retourner une liste vide si tout est valide" in {
    Invariants.checkAll(5, 10, 2, 2) shouldBe empty
  }
}