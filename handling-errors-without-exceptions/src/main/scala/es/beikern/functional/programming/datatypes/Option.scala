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

package es.beikern.functional.programming.datatypes

import scala.annotation.tailrec

sealed trait Option[+A] {
  /*
   * Implement all of the preceding functions on Option. As you implement each function, try to think about what it means and in what
   * situations you'd use it. We'll explore when to use each of these functions next. Here are a few hints for solving this exercise:
   * It's fine to use pattern matching, though you should be able to implement all the functions besides map and getOrElse without resorting
   * to pattern matching.
   * For map and flatMap, the type signature should be enough to determine the implementation.
   * getOrElse returns the result inside the Some case of the Option, or if the Option is None, returns the given default value.
   * orElse returns the first Option if it's defined; otherwise, it returns the second Option.
   */

  def map[B](f: A => B): Option[B] = this match {
    case Some(a) => Some(f(a))
    case None    => None
  }

  def flatMap[B](f: A => Option[B]): Option[B] = {
    this match {
      case Some(a) => f(a)
      case None    => None
    }
  }

  /*
   * A common idiom is to do o.getOrElse(throw new Exception("FAIL")) to con-
   * vert the None case of an Option back to an exception. The general rule of thumb is
   * that we use exceptions only if no reasonable program would ever catch the exception;
   * if for some callers the exception might be a recoverable error, we use Option (or
   * Either , discussed later) to give them flexibility.
   */

  def getOrElse[B >: A](default: => B): B = {
    this match {
      case Some(a) => a
      case None    => default
    }
  }

  def orElse[B >: A](ob: => Option[B]): Option[B] = {
    this match {
      case None  => ob
      case other => other
    }
  }

  def filter(f: A => Boolean): Option[A] = {
    this match {
      case Some(a) if !f(a) => None
      case other            => other
    }
  }
}

object Option {

  /*
   * Write a generic function map2 that combines two Option values using a binary func-
   * tion. If either Option value is None , then the return value is too.
   */
  def map2[A, B, C](a: Option[A], b: Option[B])(f: (A, B) => C): Option[C] = {
    a.flatMap(a => b.map(b => f(a, b)))
  }

  def map2ForComprehension[A, B, C](a: Option[A], b: Option[B])(f: (A, B) => C): Option[C] =
    for {
      aa <- a
      bb <- b
    } yield f(aa, bb)

  /* Write a function sequence that combines a list of Options into one Option containing
   * a list of all the Some values in the original list. If the original list contains None even
   * once, the result of the function should be None ; otherwise the result should be Some
   * with a list of all the values
   */
  def sequence[A](a: List[Option[A]]): Option[List[A]] = {
    @tailrec
    def go(l: List[Option[A]], acc: Option[List[A]]): Option[List[A]] = {
      l match {
        case Some(h) :: t => go(t, acc.map(_ :+ h))
        case None :: t    => None
        case Nil          => acc
      }
    }
    go(a, Some(List.empty[A]))

  }

  def traverse[A, B](a: List[A])(f: A => Option[B]): Option[List[B]] = {
    @tailrec
    def go(l: List[A], acc: Option[List[B]]): Option[List[B]] = {
      l match {
        case h :: t =>
          f(h) match {
            case Some(x) => go(t, acc.map(_ :+ x))
            case None    => None
          }
        case Nil => acc
      }
    }
    go(a, Some(List.empty))
  }

}

case class Some[+A](get: A) extends Option[A]
case object None extends Option[Nothing]
