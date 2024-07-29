object inventory extends App {
  
  val inventory1: Map[Int, (String, Int, Double)] = Map(
    101 -> ("apple", 10, 50.0),
    102 -> ("banana", 5, 75.0),
    103 -> ("cat", 8, 60.0)
  )

  val inventory2: Map[Int, (String, Int, Double)] = Map(
    102 -> ("bull", 3, 80.0),
    104 -> ("dog", 12, 40.0)
  )

  // I. Retrieve all product names from inventory1
  val productNames: Set[String] = inventory1.values.map(_._1).toSet
  println(s"Product names: ${productNames.mkString(", ")}")

  // II. Calculate the total value of all products in inventory1
  val totalValue: Double = inventory1.values.map { case (_, quantity, price) => quantity * price }.sum
  println(f"Total value of all products: $$${totalValue}")

  // III. Check if inventory1 is empty
  val isEmpty: Boolean = inventory1.isEmpty
  println(s"Is inventory1 empty? $isEmpty")

  // IV. Merge inventory1 and inventory2
  val mergedInventory: Map[Int, (String, Int, Double)] = (inventory1 ++ inventory2).foldLeft(Map.empty[Int, (String, Int, Double)]) {
    case (acc, (id, (name, quantity, price))) =>
      acc.get(id) match {
        case Some((_, existingQuantity, existingPrice)) =>
          acc + (id -> (name, existingQuantity + quantity, math.max(existingPrice, price)))
        case None =>
          acc + (id -> (name, quantity, price))
      }
  }

  println("Merged Inventory:")
  mergedInventory.foreach { case (id, (name, quantity, price)) =>
    println(s"ID: $id, Name: $name, Quantity: $quantity, Price: $$${price}")
  }

  // V. Check if a product with a specific ID (e.g., 102) exists in inventory1 and print its details
  val productIdToCheck = 102
  inventory1.get(productIdToCheck) match {
    case Some((name, quantity, price)) =>
      println(s"Product ID $productIdToCheck exists in inventory1.")
      println(s"Name: $name, Quantity: $quantity, Price: $$${price}")
    case None =>
      println(s"Product ID $productIdToCheck does not exist in inventory1.")
  }
}
