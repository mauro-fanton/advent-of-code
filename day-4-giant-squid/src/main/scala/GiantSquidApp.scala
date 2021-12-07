object GiantSquidApp extends App {

  val fileHandler = FileHandler()
  val boards = fileHandler.readDataFile("boards.txt")
    .split("\n\n").map(o => o.split('\n'))
    .map(o =>
       o.map(o => o.trim.split("\\s+"))
    )

  val boardsObj = boards.map(o => Board(o.map(o => o.map(_.toInt))))

  val draws = fileHandler.readDataFile("draws.txt").split(',').map(_.toInt)

  for (i <- 1 to 2) println("*********************************************************")

  println("\tDay 4 - Giant Squid Part 1:")
  println(s"\tScore is: ${Bingo().play(boardsObj, draws, 5)}")

  println("\n*********************************************************\n")

  println("\tDay 4 - Giant Squid Part 2:")
  println(s"\tScore is: ${Bingo().getLastWin(boardsObj, draws, 5)}")

  for (i <- 1 to 2) println("*********************************************************")

}
