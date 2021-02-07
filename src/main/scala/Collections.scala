package com.bootcamp.homework

import scala.annotation.tailrec

object Collections {
  // https://leetcode.com/problems/running-sum-of-1d-array/
  def runningSum(nums: Array[Int]): Array[Int] = {
    nums.zipWithIndex.map(x => x._1 + nums.take(x._2).sum)
  }

  // https://leetcode.com/problems/shuffle-the-array

  def shuffle(nums: Array[Int], n: Int): Array[Int] = {
    (0 until 2 * n).toArray.map {
      case index if index % 2 == 0 => nums(index / 2)
      case index                   => nums(n + (index / 2))
    }
  }

  // https://leetcode.com/problems/richest-customer-wealth
  def maximumWealth(accounts: Array[Array[Int]]): Int = {
    accounts.map(_.sum).max
  }

  // https://leetcode.com/problems/kids-with-the-greatest-number-of-candies/
  def kidsWithCandies(candies: Array[Int], extraCandies: Int): Array[Boolean] = {
    candies.map(sweet => sweet + extraCandies >= candies.max)
  }

  // https://leetcode.com/problems/widest-vertical-area-between-two-points-containing-no-points
  // can we use fold instead of map?
  def maxWidthOfVerticalArea(points: Array[Array[Int]]): Int = {
    val sortedX = points.map(arr => arr(0)).sorted
    sortedX.zipWithIndex.map {
      case x if x._2 > 0 => x._1 - sortedX(x._2 - 1)
      case _             => 0
    }.max
  }

  // https://leetcode.com/problems/maximum-nesting-depth-of-the-parentheses/
  def maxDepth(s: String): Int = {
    @tailrec
    def deeper(string: String, n: Int): Int = {
      string.length match {
        case 0 => n
        case 1 => n + 1
        case _ =>
          val levelLessStr = string.replace("()", "")
          levelLessStr match {
            case "" => n + 1
            case _  => deeper(levelLessStr, n + 1)
          }

      }
    }

    deeper(s.filter(a => a.equals('(') || a.equals(')')), 0)
  }

  // https://leetcode.com/problems/split-a-string-in-balanced-strings
  def balancedStringSplit(s: String): Int = {
    @tailrec
    def deeper(str: String, amount: Int, countR: Int, countL: Int): Int = {
      str.length match {
        case 0 => amount
        case _ =>
          str(0) match {
            case 'R' if countR + 1 != countL => deeper(str.substring(1), amount, countR + 1, countL)
            case 'L' if countR != countL + 1 => deeper(str.substring(1), amount, countR, countL + 1)
            case _                           => deeper(str.substring(1), amount + 1, 0, 0)
          }
      }
    }

    deeper(s, 0, 0, 0)
  }

  // https://leetcode.com/problems/matrix-block-sum/
  // TODO: algorithm can be improved (flat matrix to array?)
  def matrixBlockSum(mat: Array[Array[Int]], K: Int): Array[Array[Int]] = {
    def calculate(dem1: Int, dem2: Int): Int = {
      val rangeI = ((dem1 - K max 0) to (dem1 + K min mat.length - 1)).toArray
      val rangeJ = ((dem2 - K max 0) to (dem2 + K min mat(0).length - 1)).toArray

      rangeI.foldLeft(0)((accX, x) => accX +
        rangeJ.foldLeft(0)((accY, y) => accY + mat(x)(y)))
    }

    mat.indices.toArray map {
      i =>
        mat(0).indices.toArray map {
          j => calculate(i, j)
        }
    }
  }
}
