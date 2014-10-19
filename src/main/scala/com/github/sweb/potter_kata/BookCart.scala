package com.github.sweb.potter_kata

/**
 * Created by Florian on 18.10.2014.
 */
case class BookCart(books: List[Book]) {
  val bookNumbers = books.map(x => x.no)
  
  def sortBooksIntoSets(): List[Set[Int]] = {
    sortBooksIntoSetsIter(bookNumbers, List(Set.empty[Int]), Nil)
  }

  private def sortBooksIntoSetsIter(books: List[Int], sets: List[Set[Int]],
                                    remainingBooks: List[Int]): List[Set[Int]] = {
    if (remainingBooks.isEmpty && books.isEmpty) {
      sets
    } else {
      if (books isEmpty) {
        sortBooksIntoSetsIter(remainingBooks, Set.empty[Int] :: sets, Nil)
      } else {
        val currSet = sets.head
        val currBook = books.head
        if (!currSet.contains(currBook)) {
          sortBooksIntoSetsIter(books.tail, (currSet + currBook) :: sets.tail, remainingBooks)
        } else {
          sortBooksIntoSetsIter(books.tail, sets, currBook :: remainingBooks)
        }
      }
    }
  }

  private def smoothSets(sets: List[Set[Int]]): List[Set[Int]] = {
    //dominant pairs: (4,4) > (5,3), (4,3) > (5,2)
    val fiveBooks = sets.filter(x => x.size == 5)
    val twoBooks = sets.filter(x => x.size == 2)
    val threeBooks = sets.filter(x => x.size == 3)

    val twoAndThreeBooks = twoBooks ++ threeBooks

    if (twoAndThreeBooks isEmpty) {
      sets
    } else {
      val (queueToAdd, unmodifiedSets) = twoAndThreeBooks.splitAt(fiveBooks.size)

      val zipped: List[(Set[Int], Set[Int])] = fiveBooks.zip(queueToAdd)

      val modifiedSets = zipped.map( {x:(Set[Int], Set[Int]) => moveElement(x._1, x._2) })

      sets.filter(x => x == 1 || x == 4) ++ modifiedSets.map(x => x._1) ++ modifiedSets.map(x => x._2) ++ unmodifiedSets ++ fiveBooks.drop(zipped.size)
    }
  }

  private def moveElement(source: Set[Int], target: Set[Int]): (Set[Int], Set[Int]) = {
    (source - 5, target + 5)
  }
  val bookSets = sortBooksIntoSets()
  val bookSetsAfterSmoothing = smoothSets(bookSets)

  def determinePrice(set: Set[Int]): Double = {
    val factor = set.size match {
      case 0 => 1
      case 1 => 1
      case 2 => 0.95
      case 3 => 0.9
      case 4 => 0.8
      case 5 => 0.75
    }
    8 * set.size * factor
  }

  def determineTotalPrice() = {
    bookSetsAfterSmoothing.map(x => determinePrice(x)).sum
  }

}