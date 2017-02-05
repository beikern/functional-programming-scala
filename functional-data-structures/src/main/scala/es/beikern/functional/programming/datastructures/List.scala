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

import scala.annotation.tailrec

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

  /*
   * Exercise 3.3
   *
   * Using the same idea, implement the function setHead for replacing the first element of a List with a different
   * value.
   */
  def setHead[B >: A](e: B): List[B] = {
    this match {
      case Cons(h, t) => Cons(e, Cons(h, t))
      case Nil        => Cons(e, Nil)
    }
  }

  /*
   * Exercise 3.4
   *
   * Generalize tail to the function drop, which removes the first n elements from a list.
   * Note that this function takes time proportional only to the number of elements being dropped - we don't need to make a copy
   * of the entire List.
   */
  def drop(n: Int): List[A] = {
    @tailrec
    def go(i: Int, l: List[A]): List[A] = {
      i match {
        case 0                                => l
        case lessThanZero if lessThanZero < 0 => throw new IllegalArgumentException("Elements to drop should be greater than 0")
        case other                            => go(i - 1, l.tail)
      }
    }
    go(n, this)
  }

  /*
   * Exercise 3.5
   *
   * Implement dropWhile, which removes elements from the List prefix as long as they match a predicate
   */
  def dropWhile(f: A => Boolean): List[A] = {
    @tailrec
    def go(l: List[A]): List[A] = {
      l match {
        case Cons(h, t) if f(h) => go(t)
        case x                  => x
      }
    }
    go(this)
  }

  def addToTail[B >: A](a: B): List[B] = {
    def go(l: List[B]): List[B] = {
      l match {
        case Cons(h, t) => go(t).setHead(h)
        case Nil        => Nil.setHead(a)
      }
    }
    go(this)
  }
  /*
   * Exercise 3.6
   *
   * Not everything works out so nicely. Implement a function, init, that returns a List consisting of all but the last element
   * of a List. So, given List(1,2,3,4), init will return List(1,2,3). Why can't this function be implemented in constant time
   * like tail?
   *
   * We are implementing here a linked list. To remove the last element we have to iterate over n-1 elements, so it is linear
   * time, not constant.
   *
   * WARNING: This implementation is not tail recursive! it can overflow the stack if the list is big enough!
   */
  def init: List[A] = {
    def go(l: List[A]): List[A] = {
      l match {
        case Cons(p, Cons(u, Nil)) => Nil.setHead(p)
        case Cons(h, Nil)          => Nil
        case Cons(h, t)            => go(t).setHead(h)
        case Nil                   => throw new UnsupportedOperationException("List has to have at least one element")
      }
    }
    go(this)
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
