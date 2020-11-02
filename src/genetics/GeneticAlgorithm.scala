package genetics

import scala.annotation.tailrec

object GeneticAlgorithm {

  /**
    * Uses a genetic algorithm to optimize a generic problem
    *
    * @param incubator Determines how instances of type T are created from a List of Doubles (genes)
    * @param costFunction Determines the cost for a given instance of T
    * @param numberOfGenes The size of the List expected by the incubator
    * @tparam T The type to be optimized
    * @return An instance of T with minimal cost
    */
  def geneticAlgorithm[T](incubator: List[Double] => T, costFunction: T => Double, numberOfGenes: Int): T = {

    // Generate random [Lists of Double] (Animals)
    val animals: List[List[Double]] = animalsGenerator(animals, 10, numberOfGenes, -100, 100)
    val listOfT: List[T] = animals.map(incubator(_)) // Convert "potential solution" from following index
    val listOfCost: List[Double] = listOfT.map(costFunction(_)) // compute the cost of each potential solution

    // Creating connection map (animal -> animal's costs)
    val cost_animals_connection = new scala.collection.mutable.HashMap[List[Double], Double]
    for (index <- listOfCost.indices) {
      cost_animals_connection += (animals(index) -> listOfCost(index))
    }

    // Sort the map by second value "cost"
    val mapSorted = cost_animals_connection.toList.sortBy(_._2)

    // Get the best animal in this generation
    val bestAnimal: List[Double] = mapSorted.head._1

    // Simulate genetic mutations of the best animals
    val bestAnimals: List[List[Double]] = animalsGenerator(bestAnimals, 10, numberOfGenes, bestAnimal.min - 5, bestAnimal.max + 5)

    // Pair them / create offspring by combining their genes
    val paired: List[List[Double]] = pair(bestAnimals, paired, bestAnimals.length / 2)
    val moreRandomAnimals: List[List[Double]] = animalsGenerator(moreRandomAnimals, 10 - paired.length, numberOfGenes, -100, 100)
    val new_animals = paired ::: moreRandomAnimals
    incubator(bestAnimal)

    if (costFunction(geneticAlgorithm(incubator, costFunction, numberOfGenes)) < 1){
      geneticAlgorithm(incubator, costFunction, numberOfGenes)
    } else {
      geneticAlgorithm(incubator, costFunction, numberOfGenes)
    }

  }

  @tailrec
  def animalsGenerator(inputData: List[List[Double]], numberOfAnimals: Int, numberOfGenes: Int, minSize: Double, maxSize: Double): List[List[Double]] = {
    if(numberOfAnimals == 0){
      inputData
    } else {
      inputData :+ List.fill(numberOfGenes)(minSize + (maxSize - minSize) * scala.util.Random.nextDouble)
      animalsGenerator(inputData, numberOfAnimals - 1, numberOfGenes, minSize, maxSize)
    }
  }

  @tailrec
  def pair(inputData: List[List[Double]], outputData: List[List[Double]], index: Int): List[List[Double]] = {
    if (index == 0){
      inputData
    } else {
      val (left, right) = inputData.splitAt(index)
      outputData :+ List((left(index).head + right(index).head) / 2, (left(index)(1) + right(index)(1)) / 2)
      pair(inputData, outputData, index - 1)
    }
  }

}
