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

import es.beikern.functional.programming.datatypes.{ Some, None }
import es.beikern.functional.programming.datatypes.Option
import es.beikern.functional.programming.datatypes.Either
import es.beikern.functional.programming.datatypes.{Right, Left}
import es.beikern.functional.programming.app.Utils._

object Main extends App {

  val mapSomeResult = Some(1).map(_ + 1)
  println(s"the Some map result is $mapSomeResult")

  val flatMapSomeResult = Some(Some(1)).flatMap(identity)
  println(s"The flatMap result is $flatMapSomeResult")

  val getOrElseResult = None.getOrElse(1)
  println(s"The getOrElse result is $getOrElseResult")

  val noneOrElseResult = None.orElse(Some(1))
  println(s"the None orElse result is $noneOrElseResult")

  val filterResult = Some(10).filter(_ % 2 == 1)
  println(s"The filter result is $filterResult")

  println(s"variance is ${variance(List(1.0, 2.0, 3.0))}")

  val eitherRight: Either[Boolean, Int] = Right(1)

  val eitherRightMapResult = eitherRight.map(_ + 1)
  println(s"The Either map right result is $eitherRightMapResult")

  val eitherLeft: Either[Boolean, Int] = Left(false)

  val eitherLeftMapResult = eitherLeft.map(_ + 1)
  println(s"The Either map left result is $eitherLeftMapResult")


  /**
    * Top secret formula for computing an annual car
    * insurance premium from two key factors.
    */
  def insuranceRateQuote(age: Int, numberOfSpeedingTickets: Int): Double = {
    (age + numberOfSpeedingTickets) / 2
  }

  def parseInsuranceRateQuote(age: String, numberOfSpeedingTickets: String): Option[Double] = {
    val optAge: Option[Int]     = Try { age.toInt }
    val optTickets: Option[Int] = Try { numberOfSpeedingTickets.toInt }
    Option.map2(optAge, optTickets)(insuranceRateQuote)
  }

  println(s"the parseInsuranceRateQuote result is : ${parseInsuranceRateQuote("wololo", "10")}")
  println(s"The option sequence result is ${Option.sequence(List(Some(1), Some(2), Some(3)))}")
  println(s"The option sequence result using traverse is ${Option.sequenceUsingTraverse(List(Some(1), Some(2), Some(3)))}")
  println(
    s"The traverse result with some None when evaluating the function is ${Option.traverse(List(1, 2, 3, 4))(a => if (a % 2 == 0) { Some(a) } else None)}")
}
