package com.github.sweb.potter_kata

import org.scalatest.FlatSpec

/**
 * Created by Florian on 18.10.2014.
 */
class BookCartSpec extends FlatSpec{
  "A BookCart" should "oder books into sets" in {
    val nums = List(1,1,2,2,3,3,4,5)
    val books = nums.map(x => Book(x))
    val bc = BookCart(books)
    val res = Set(Set(1,2,3,4,5),Set(1,2, 3))
    assert(bc.sortBooksIntoSets().toSet == res)
  }

  it should "calculate the total price" in {
    val nums = List(1,1,2,2,3,3,4,5)
    val books = nums.map(x => Book(x))
    val bc = BookCart(books)
    val totalPrice = bc.determineTotalPrice()
    val res = 51.2

    assert(totalPrice == res)
  }

  it should "calculate edge case 1" in {
    val nums = List(1,1,1,1,1,2,2,2,2,2,3,3,3,3,4,4,4,4,4,5,5,5,5)
    val books = nums.map(x => Book(x))
    val bc = BookCart(books)
    val totalPrice = bc.determineTotalPrice()
    val res = 141.2

    assert(totalPrice == res)
  }

}
