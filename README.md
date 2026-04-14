# Projet Programmation Fonctionnelle 2026 : Ant Colony

## Overview

Ce projet simule un **système critique distribué** en utilisant le modèle
d'acteurs (Akka + Scala 2). La survie de la colonie repose sur un invariant
critique : **la Reine doit rester en vie**. Si sa faim dépasse le seuil
maximum, la colonie s'effondre.

L'objectif académique est de modéliser ce système avec des acteurs Akka,
puis de le traduire formellement en réseau de Pétri pour vérifier
l'absence de deadlocks et le respect des invariants métier.

## Architecture des acteurs

| Acteur | Rôle |
|--------|------|
| `QueenActor` | Composant critique. Gère sa faim, pond des œufs, supervise les naissances et décès de fourmis |
| `EggActor` | Représente un œuf en incubation. Éclot après 2 cycles en fourrageuse ou transporteuse |
| `ForagerAntActor` | Cherche de la nourriture à l'extérieur et la dépose dans le stockage |
| `CarrierAntActor` | Prend la nourriture du stockage et la livre à la reine |
| `StorageActor` | Buffer partagé entre fourrageuses et transporteuses |
| `ColonyGuardianActor` | Superviseur racine — définit les stratégies de supervision par acteur |
| `Simulation` | Lance les acteurs initiaux et le scheduler de la reine |
| `SimulationMonitor` | Observe la reine via `watch` + illustre le pattern MessageAdapter |

## Flux de messages critiques

![diagramme.png](diagramme.png)

## Invariants métier actuels

| Invariant | Vérifié dans |
|-----------|-------------|
| `hunger ∈ [0, MaxHunger=10]` | `Invariants.checkQueen` à chaque `Tick` |
| Stock non négatif | Structurellement — le stockage ne livre que ce qu'il a |
| Stock `≤ MaxCapacity=20` | `Invariants.checkStorage` à chaque `DepositFood` |
| `foragerCount ∈ [0, MaxPerType=4]` | `Invariants.checkPopulation` à chaque `SpawnAnt`/`AntDied` |
| `carrierCount ∈ [0, MaxPerType=4]` | `Invariants.checkPopulation` à chaque `SpawnAnt`/`AntDied` |
| Une fourmi morte notifie toujours la reine | `AntDied` envoyé avant `Behaviors.stopped` |


## Installation & Exécution

### Prérequis
* SBT 1.X (Scala Build Tool)
* Java JDK 21
* Scala 2.13

### Lancer la simulation
Le projet utilise un workflow standard SBT. Pour compiler et lancer le système :

```bash
sbt run
```

Appuie sur **Entrée** pour arrêter proprement le système.

### Lancer les tests

```bash
sbt test
```

### Lancer l'analyseur de réseau de Pétri

```bash
sbt "runMain PetriNetMain"
```

### Nettoyer les builds

```bash
sbt clean
```

À faire si les builds s'accumulent ou si tu changes de version de dépendances.

### Structure des fichiers source

```
app/src/main/
├── scala/
│   ├── Main.scala                  # Point d'entrée de l'application
│   ├── petri.scala                 # Réseau de Pétri P/T + analyseur BFS + LTL
│   ├── actors/                     # Logique des acteurs (FSM)
│   │   ├── QueenActor.scala
│   │   ├── EggActor.scala
│   │   ├── ForagerAntActor.scala
│   │   ├── CarrierAntActor.scala
│   │   ├── StorageActor.scala
│   │   └── ColonyguardianActor.scala
│   ├── domain/                     # Logique métier et invariants
│   │   └── Invariants.scala
│   ├── protocol/                   # Définition des messages (Case Classes/Objects)
│   │   └── Messages.scala
│   └── simulation/                 # Setup du système d'acteurs
│       ├── Simulation.scala
│       └── SimulationLogger.scala  # Logger JSON Lines (simulation_log.jsonl)
└── resources/
    └── logback.xml                 # Configuration de la journalisation SLF4J
 
app/src/test/scala/
├── actors/
│   └── ActorSpec.scala             # Tests unitaires des acteurs Akka
└── PetrinetSpec.scala              # Tests unitaires du réseau de Pétri
```
