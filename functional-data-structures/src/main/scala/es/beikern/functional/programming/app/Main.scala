/*
 * Copyright 2017 Beikern
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *     http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */

package es.beikern.functional.programming.app
import es.beikern.functional.programming.datastructures.{ Cons, List }
import es.beikern.functional.programming.datastructures.List._
import es.beikern.functional.programming.datastructures.Nil

import scala.util.{ Failure, Success, Try }

object Main extends App {

  /*
   * Exercise 3.1
   *
   * What will be the result of the following match expression?
   * My response: 3
   *
   * First case will not match, because x will match with 1, next element is 2 and next should be 4 whereas our list has a 3
   * Second case will not match, our list is not empty.
   * Third case MATCH!
   * Fourth case: Third case matched first, so this will not match.
   */
  val resultPatternMatching = List(1, 2, 3, 4, 5) match {
    case Cons(x, Cons(2, Cons(4, _)))          => x
    case Nil                                   => 42
    case Cons(x, Cons(y, Cons(3, Cons(4, _)))) => x + y
    case Cons(h, t)                            => h + sum(t)
  }
  println(s"the result is 3 right? ${ assert(resultPatternMatching == 3); resultPatternMatching }")

  val resultTailNonEmptyList = List(1, 2, 3, 4, 5).tail
  println(s"The result resultTailNonEmptyList should be Cons(2, Cons(3, Cons(4, Cons(5, Nil)))). Result: ${
    assert(Cons(2, Cons(3, Cons(4, Cons(5, Nil)))) == resultTailNonEmptyList); resultTailNonEmptyList
  }")

  Try {
    List().tail
  } match {
    case Success(_) =>
      println("WHAT! Why this happened! What sorcery is this?!")
    case Failure(ex: UnsupportedOperationException) =>
      println(s"Exception UnsupportedOperationException throwed as expected")
    case Failure(_) =>
      println(s"WHAT! (again, yep) this is not the expected error!")
  }

  val setHeadToList = List(1, 2).setHead(3)
  println(s"The result setHeadToList should be ${ assert(Cons(3, Cons(1, Cons(2, Nil))) == setHeadToList); setHeadToList }")

  val dropElemList = List(1, 2, 3, 4, 5).drop(2)
  println(s"The result dropElemList should be ${ assert(Cons(3, Cons(4, Cons(5, Nil))) == dropElemList); dropElemList }")

  val dropWhileList = List(1, 2, 3, 4).dropWhile(_ < 3)
  println(s"The result dropWhileList should be ${ assert(Cons(3, Cons(4, Nil)) == dropWhileList); dropWhileList }")

}
