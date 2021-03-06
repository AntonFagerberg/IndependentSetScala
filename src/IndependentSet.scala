object IndependentSet {
  var recursiveCalls = 0

  def printMatrix(matrix: List[List[Int]]): Unit = {
    matrix.foreach(row => println(row.mkString(" | ")))
  }

  def addVertex(matrix: List[List[Int]], neighbours: List[Int]): List[List[Int]] = {
    matrix
      .zipWithIndex
      .map {
        case (row, index) => row ::: List(if (neighbours.contains(index)) 1 else 0)
      } ::: List(
        List.fill(matrix.length + 1)(0)
          .zipWithIndex
          .map { case (_, index) => if (neighbours.contains(index)) 1 else 0 }
      )
  }

  def dropVertices(matrix: List[List[Int]], vertexNumbers: List[Int]): List[List[Int]] = {
    if (vertexNumbers.isEmpty) matrix
    else dropVertices(dropVertex(matrix, vertexNumbers.head), vertexNumbers.tail)
  }

  def dropVertex(matrix: List[List[Int]], vertexNumber: Int): List[List[Int]] = {
    (matrix.slice(0, vertexNumber) ::: matrix.drop(vertexNumber + 1))
      .map(row => row.slice(0, vertexNumber) ::: row.drop(vertexNumber + 1))
  }

  def dropVertexWithNeighbour(matrix: List[List[Int]], vertexNumber: Int): List[List[Int]] = {
    val neighbours =
      matrix(vertexNumber)
        .zipWithIndex
        .filter { case (value, index) => value == 1 || index == vertexNumber }
        .unzip
        ._2
        .sorted
        .reverse

    dropVertices(matrix, neighbours)
  }

  def independentSet(matrix: List[List[Int]]): Int = {
    recursiveCalls += 1

    if (matrix.isEmpty) 0
    else {
      val indexedMatrix = matrix.zipWithIndex

      indexedMatrix
        .find(_._1.sum == 2)
        .map { case (row, vertexNumber) =>
          val neighbours =
            matrix(vertexNumber)
              .zipWithIndex
              .filter(_._1 == 1)
              .unzip._2

          if (matrix(neighbours(0))(neighbours(1)) == 1) {
            1 + independentSet(dropVertexWithNeighbour(matrix, vertexNumber))
          } else {
            val newVertexNeighbours =
              (   matrix(neighbours(0))
                    .zipWithIndex
                    .filter {case (value, index) => value == 1 && index != vertexNumber && !neighbours.contains(index) }
                    .unzip
                    ._2
                :::
                  matrix(neighbours(1))
                    .zipWithIndex
                    .filter { case (value, index) => value == 1 && index != vertexNumber && !neighbours.contains(index) }
                    .unzip
                    ._2
              ).distinct

            1 +
              independentSet(
                dropVertexWithNeighbour(
                  addVertex(
                    matrix,
                    newVertexNeighbours
                  ),
                  vertexNumber
                )
              )
          }
        }.getOrElse {
          indexedMatrix
            .find(_._1.sum == 1)
            .map { case (row, vertexNumber) =>
              1 + independentSet(dropVertexWithNeighbour(matrix, vertexNumber))
            }.getOrElse {
              indexedMatrix
                .find(_._1.sum == 0)
                .map { case (row, vertexNumber) =>
                  1 + independentSet(dropVertex(matrix, vertexNumber))
                }.getOrElse {
                  val maxDegreeVertex =
                    indexedMatrix
                      .maxBy(_._1.sum)
                      ._2

                  Math.max (
                    1 + independentSet(
                      dropVertexWithNeighbour(
                        matrix,
                        maxDegreeVertex
                      )
                    ),
                    independentSet(
                      dropVertex(
                        matrix,
                        maxDegreeVertex
                      )
                    )
                  )
                }
              }
        }
    }
  }

  def main(args: Array[String]): Unit = {
    List(
//      "data/g4.in",
      "data/g30.in",
      "data/g40.in",
      "data/g50.in",
      "data/g60.in",
      "data/g70.in",
      "data/g80.in",
      "data/g90.in",
      "data/g100.in",
      "data/g110.in",
      "data/g120.in",
      "data/g130.in"
    ).foreach { fileName =>
      val matrix =
        io.Source.fromFile(fileName)
          .getLines()
          .drop(1)
          .map(
            _.split(" ")
            .map(_.toInt)
            .toList
          )
          .toList

      val startTime = System.currentTimeMillis
      recursiveCalls = 0
      println(s"Filename: $fileName. Independent set: ${independentSet(matrix)}. Running time: ${System.currentTimeMillis - startTime}ms. Recursive calls: $recursiveCalls.")
    }
  }
}