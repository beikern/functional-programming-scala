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

object Main extends App {

  /*
   * What will be the result of the following match expression?
   * My response: 3
   *
   * First case will not match, because x will match with 1, next element is 2 and next should be 4 whereas our list has a 3
   * Second case will not match, our list is not empty.
   * Third case MATCH!
   * Fourth case: Third case matched first, so this will not match.
   */
  val result = List(1, 2, 3, 4, 5) match {
    case Cons(x, Cons(2, Cons(4, _)))          => x
    case Nil                                   => 42
    case Cons(x, Cons(y, Cons(3, Cons(4, _)))) => x + y
    case Cons(h, t)                            => h + sum(t)
  }
  println(s"the X result is 3 right? ${ assert(result == 3); result }")
}
