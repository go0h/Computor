package fr.fourtytwo

import scala.collection.immutable.Map
import fr.fourtytwo.math._

package object expression {

  val OPERATORS: Map[String, (Double, Double) => Double] =
    Map("+" -> (_ + _), "-" -> (_ - _), "*" -> (_ * _),
      "/" -> division, "%" -> modulo, "^" -> pow)

}
