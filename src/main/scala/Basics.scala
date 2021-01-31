package com.bootcamp.homework

import scala.annotation.tailrec

object Basics {
  @tailrec
  def gcd(a: Int, b: Int): Int = (a, b) match {
    case (0, _) => b
    case (_, _) => gcd(b % a, a)
  }

  def lcm(a: Int, b: Int): Int = Math.abs(a * b) / gcd(a, b)
}
