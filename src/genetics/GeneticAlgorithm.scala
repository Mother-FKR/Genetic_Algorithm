package genetics

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
    // val animal: List[Double] = List.fill(numberOfGenes)(-100 + (100 + 100) * scala.util.Random.nextDouble)
    val animals: List[List[Double]] = animalsGenerator(10, numberOfGenes)
    val listOfT: List[T] = animals.map(incubator(_)) // Convert "potential solution" from following index
    val listOfCost: List[Double] = listOfT.map(costFunction(_)) // compute the cost of each potential solution

    // Creating connection map (animal -> animal's costs)
    val cost_animals_connection = new scala.collection.mutable.HashMap[List[Double], Double]
    for (index <- listOfCost.indices){
      cost_animals_connection += (animals(index) -> listOfCost(index))
    }

    // Sort the map by second value "cost"
    val mapSorted = cost_animals_connection.toList.sortBy(_._2)

    // Get the best animal in this generation
    val bestAnimal: List[Double] = mapSorted.head._1




  }

  def animalsGenerator(numberOfAnimals: Int, numberOfGenes: Int): List[List[Double]] = {
    val animals: List[List[Double]] = List()
    if(numberOfAnimals == 0){
      animals
    } else {
      animals :+ List.fill(numberOfGenes)(-100 + (100 + 100) * scala.util.Random.nextDouble)
      animalsGenerator(numberOfAnimals - 1, numberOfGenes)
    }
  }

  def comparator()

}
