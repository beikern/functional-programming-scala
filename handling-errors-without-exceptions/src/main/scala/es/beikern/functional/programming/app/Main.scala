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

}
