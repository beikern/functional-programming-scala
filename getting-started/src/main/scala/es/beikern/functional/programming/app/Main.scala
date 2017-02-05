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

import scala.annotation.tailrec

object Main extends App {

  /*
   * Write a recursive function to get the nth Fibonacci number ( http://mng.bz/C29s ) the first Fibonacci numbers are 0 and 1.
   * The nth number is always the sum of the previous two. The sequence begins 0, 1, 1, 2, 3, 5. Your definition should use a
   * local tail-recursive function.
   */
  def fib(n: Int): Int = {

    @tailrec
    def run(actual: Int, acc: Int, next: Int): Int = {
      if (actual == n) {
        acc
      } else {
        run(actual + 1, next, acc + next)
      }
    }

    // Running the local tail recursive function.
    run(0, 0, 1)
  }

  assert(fib(10) == 55)
  println(s"the fibonacci number is ${fib(10)}")

  /*
   * Implement isSorted, which checks whether an Array[A] is sorted according to a given comparison function
   */
  def isSorted[A](as: Array[A], ordered: (A, A) => Boolean): Boolean = {
    @tailrec
    def run(actualIndex: Int = 0, acc: Boolean): Boolean = {
      val maxIndex = as.length - 1
      if (as.length < 2) {
        true
      } else {
        actualIndex match {
          case index if index > maxIndex => acc
          case _ =>
            run(actualIndex + 1, acc && ordered(as(actualIndex), as(actualIndex + 1)))

        }
      }
    }

    // Running the local tailrec function
    run(acc = true)
  }

  println(s"is it sorted? ${isSorted[Int](Array(1, 4, 3, 4), (a, b) => a <= b)}")

  /*
   * Let's look at another example, currying, which converts a function f of two arguments into a function of one argument that
   * partially applies f. Here again there's only one implementation that compiles. Write that implementation.
   *
   * La clave está en seguir los tipos, es decir, ¿Qué me pide la función destino? pues empiezo escribiendo eso como entrada de mi
   * función anónima y de ahí voy tirando del hilo
   */

  def curry[A, B, C](f: (A, B) => C): A => (B => C) = { (a: A) =>
    { (b: B) =>
      f(a, b)
    }
  }

  /*
   * Implement uncurry, which reverses the transformation of curry. Note that since => associates to the right, A => (B => C) can
   * be written as A => B => C
   *
   * La clave está en seguir los tipos, es decir, ¿Qué me pide la función destino? pues empiezo escribiendo eso como entrada de mi
   * función anónima y de ahí voy tirando del hilo
   */

  def uncurry[A, B, C](f: A => B => C): (A, B) => C = { (a: A, b: B) =>
    {
      f(a)(b)
    }
  }

  /*
   * Implement the high order function that composes two functions
   * La clave está en seguir los tipos, es decir, ¿Qué me pide la función destino? pues empiezo escribiendo eso como entrada de mi
   * función anónima y de ahí voy tirando del hilo
   */
  def compose[A, B, C](f: B => C, g: A => B): A => C = { (a: A) =>
    f(g(a))
  }
}
