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
import es.beikern.functional.programming.datastructures.{ Branch, Cons, Leaf, List, Nil, Tree }
import es.beikern.functional.programming.datastructures.List._

import scala.util.{ Failure, Success, Try }

// TODO beikern: This should be Scala Tests! not a dirty main :/ Refactor ASAP.
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
    case Failure(_: UnsupportedOperationException) =>
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

  val initList = List(1, 2, 3, 4).init
  println(s"The result initList should be ${ assert(Cons(1, Cons(2, Cons(3, Nil))) == initList); initList }")

  val foldRightList = List(1, 2, 3, 4).foldRight(0)((a, b) => b + a)
  println(s"The sum result is ${ assert(foldRightList == 10); foldRightList }")

  val lengthList = List('a', 'b', 'c', 'd', 'e').length
  println(s"The length is ${ assert(lengthList == 5); lengthList }")

  val sumList = List(1, 2, 3, 4, 5).sumFoldLeft
  println(s" The sum using sumFoldLeft is ${ assert(sumList == 15); sumList }")

  val lengthFoldLeftList = List(1, 2, 3, 4, 5).lengthFoldLeft
  println(s" The length using lengthFoldLeft is ${ assert(lengthFoldLeftList == 5); lengthFoldLeftList }")

  val productList = List(1, 2, 3, 4, 5).productFoldLeft
  println(s" The length using productFoldLeft is ${ assert(productList == 120); productList }")

  val reverseList = List(1, 2, 3, 4).reverse
  println(s"The list reversed is $reverseList")

  val mapList: List[String] = List(1, 2, 3, 4).map(_.toString.concat("_wololo"))
  println(s"The list mapped to List[String] is $mapList")

  val filterList: List[Int] = List(1, 2, 3, 4).filter(_ % 2 == 1)
  println(s"The list filtered is $filterList")

  val concatList: List[Int] = List(1, 2, 3, 4).concat(List(1, 2, 3, 4))
  println(s"Concat result: $concatList")

  val flatmaplist = List(1, 2, 3).flatMap(i => List(i, i))
  println(s"flatmap result = $flatmaplist")

  val filterUsingFlatMapList = List(1, 2, 3, 4).filterUsingFlatMap(_ % 2 == 1)
  println(s"The list filtered using flatmap is $filterUsingFlatMapList")

  val zipWith = List(1, 2, 3, 4).zipWith(List(1, 2, 3, 4), _ + _)
  println(s"zipWithResult = $zipWith")

  /*
   *  +----------------------+
   *  |     Example Tree     |
   *  |                      |
   *  |           º          |
   *  |           |          |
   *  |       +-------+      |
   *  |       |       |      |
   *  |   +---º---+   +      |
   *  |   |       |   3      |
   *  |   +       +          |
   *  |   1       2          |
   *  +----------------------+
   *
   */

  val tree1 = Tree(Branch(Branch(Leaf(1), Leaf(2)), Leaf(3)))

  val treeSize = tree1.size
  println(s"the tree size (nodes + leafs) is $treeSize")

  val treeMax = tree1.maximum
  println(s"the tree maximum is ${ assert(treeMax == 3); treeMax }")

  /*
   *  +--------------------------------+
   *  |        Example Tree  2         |
   *  |                                |
   *  |                    º           |
   *  |                    |           |
   *  |                +---+---+       |
   *  |                |       |       |
   *  |            +---º---+   3       |
   *  |            |       |           |
   *  |            +       2           |
   *  |        +---º---+               |
   *  |        |       |               |
   *  |    +---º---+   4               |
   *  |    |       |                   |
   *  |    5       6                   |
   *  |                                |
   *  +--------------------------------+
   *
   */
  val tree2 = Tree(Branch(Branch(Branch(Branch(Leaf(5), Leaf(6)), Leaf(4)), Leaf(2)), Leaf(3)))

  val treeMaxDepth = tree2.depth
  println(s"the tree maximum depth is ${ assert(treeMaxDepth == 5); treeMaxDepth }")

  val mappedTree = tree1.map(_ + 1)
  println(s"the mapped tree is $mappedTree")

  val treeSizeFold = tree1.sizeFold
  println(s"the tree size (nodes + leafs) sizeFold is $treeSizeFold")
}
