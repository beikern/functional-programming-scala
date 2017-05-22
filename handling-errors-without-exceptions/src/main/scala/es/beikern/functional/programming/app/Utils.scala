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

import es.beikern.functional.programming.datatypes.Option
import es.beikern.functional.programming.datatypes.{ Some, None }

object Utils {
  def variance(xs: Seq[Double]): Option[Double] = {
    if (xs.isEmpty) {
      None
    } else {
      val mean = xs.sum / xs.size
      Some(xs.map(x => math.pow(x - mean, 2)).sum / xs.size)
    }
  }

  def Try[A](a: => A): Option[A] =
    try Some(a)
    catch { case e: Exception => None }

}
