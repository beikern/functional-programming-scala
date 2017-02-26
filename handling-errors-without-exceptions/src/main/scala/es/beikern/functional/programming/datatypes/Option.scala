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
case class Some[+A](get: A) extends Option[A]
case object None extends Option[Nothing]
