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

package es.beikern.functional.programming.datastructures

sealed trait List[+A] {
  /*
   * Exercise 3.2
   *
   * Implement the function tail for removing the first element of a List. Note that the function takes constant time. What are
   * different choices you could make in your implementation if the List is Nil? We'll return to this question in the next chapter.
   *
   * Answer: This implementation throws an exception. We can return an Optional or even Nil
   * This method is here instead of the companion because it sucks to call tail like this: List.tail(List(1,2,3,4)), this is a better
   * place for it
   */
  def tail: List[A] = {
    this match {
      case Cons(_, t) => t
      case Nil        => throw new UnsupportedOperationException("tail of empty list!")
    }
  }
}
case object Nil                       extends List[Nothing]
case class Cons[+A](h: A, t: List[A]) extends List[A]

object List {
  def sum(ints: List[Int]): Int = ints match {
    case Nil         => 0
    case Cons(x, xs) => x + sum(xs)
  }

  def product(ds: List[Double]): Double = ds match {
    case Nil          => 1.0
    case Cons(0.0, _) => 0.0
    case Cons(x, xs)  => x * product(xs)
  }

  def apply[A](as: A*): List[A] =
    if (as.isEmpty) Nil
    else Cons(as.head, apply(as.tail: _*))
}
