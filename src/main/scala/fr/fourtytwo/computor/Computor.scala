package fr.fourtytwo.computor

import scala.collection.mutable.{Map => MMap}
import scala.math.pow

class Computor {

  val operators: MMap[String, (Double, Double) => Double] =
    MMap("+" -> (_ + _), "-" -> (_ - _), "*" -> (_ * _),
         "/" -> (_ / _), "%" -> (_ % _),
         "^" -> ((a: Double, b: Double) => pow(a, b)))

  val variables: MMap[String, Double] = MMap[String, Double]()

}
