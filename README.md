# Projet Programmation Fonctionnelle 2026 : Ant Colony

## Overview
Ce projet simule un **système critique distribué** en utilisant le modèle d'acteurs (Akka + Scala).
La survie de la colonie dépend d'une ressource critique : **la Reine**. Si sa faim dépasse un certain seuil, le système s'arrête (échec critique).

L'objectif est de modéliser et de vérifier les comportements de la colonie à l'aide de protocoles de communication asynchrones.

## Architecture

* **QueenActor** : Composant critique du système. Gère son propre état de faim de manière autonome.
* **WorkerAntActor** : Simule les ouvrières qui cherchent des ressources pour stabiliser le système.
* **Simulation** : Orchestrateur qui initialise le système et gère les cycles de vie.

## Installation & Exécution

### Prérequis
* SBT (Scala Build Tool)
* Java JDK 21

### Lancer la simulation
Le projet utilise un workflow standard SBT. Pour compiler et lancer le système :

```bash
sbt run