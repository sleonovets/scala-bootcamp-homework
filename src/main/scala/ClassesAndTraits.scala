package com.bootcamp.homework

object ClassesAndTraits {

  sealed trait Shape2D extends Located2D with Bounded2D {
    def area: Double
  }

  sealed trait Shape3D extends Located3D with Bounded3D {
    def surfaceArea: Double

    def volume: Double
  }

  sealed trait Located2D {
    def x: Double

    def y: Double
  }

  sealed trait Located3D extends Located2D {
    def z: Double
  }

  object Origin2D extends Located2D {
    override def x: Double = 0

    override def y: Double = 0
  }

  object Origin3D extends Located3D {
    override def x: Double = 0

    override def y: Double = 0

    override def z: Double = 0
  }

  sealed trait Bounded2D {
    def minX: Double

    def maxX: Double

    def minY: Double

    def maxY: Double
  }

  sealed trait Bounded3D extends Bounded2D {
    def minZ: Double

    def maxZ: Double
  }

  sealed trait Movable2D {
    def move(dx: Double, dy: Double): Shape2D
  }

  sealed trait Movable3D {
    def move(dx: Double, dy: Double, dz: Double): Shape3D
  }

  sealed trait Point {
    def minX: Double

    def maxX: Double

    def minY: Double

    def maxY: Double
  }

  final case class Point2D(x: Double, y: Double) extends Point {
    override def minX: Double = x

    override def maxX: Double = x

    override def minY: Double = y

    override def maxY: Double = y
  }

  final case class Point3D(x: Double, y: Double, z: Double) extends Point {
    override def minX: Double = x

    override def maxX: Double = x

    override def minY: Double = y

    override def maxY: Double = y

    def minZ: Double = z

    def maxZ: Double = z
  }

  final case class Triangle(points: Set[Point2D]) extends Shape2D {
    override def x: Double = points.map(_.x).sum / 3

    override def y: Double = points.map(_.y).sum / 3

    override def minX: Double = points.map(_.x).min

    override def maxX: Double = points.map(_.x).max

    override def minY: Double = points.map(_.y).min

    override def maxY: Double = points.map(_.y).max

    override def area: Double = ???
  }

  final case class Square(points: Set[Point2D]) extends Shape2D {
    override def x: Double = points.map(_.x).sum / 4

    override def y: Double = points.map(_.y).sum / 4

    override def minX: Double = points.map(_.x).min

    override def maxX: Double = points.map(_.x).max

    override def minY: Double = points.map(_.y).min

    override def maxY: Double = points.map(_.y).max

    override def area: Double = ???
  }

  final case class Circle3D(centerX: Double, centerY: Double, centerZ: Double, radius: Double) extends Shape3D {
    override def x: Double = centerX

    override def y: Double = centerY

    override def z: Double = centerZ

    override def minX: Double = x - radius

    override def maxX: Double = x + radius

    override def minY: Double = y - radius

    override def maxY: Double = y + radius

    override def minZ: Double = y - radius

    override def maxZ: Double = y + radius

    override def surfaceArea: Double = ???

    override def volume: Double = ???
  }

  final case class Cube(points: Set[Point3D]) extends Shape3D {
    override def x: Double = points.map(_.x).sum / 4

    override def y: Double = points.map(_.y).sum / 4

    override def z: Double = points.map(_.z).sum / 4

    override def minX: Double = points.map(_.x).min

    override def maxX: Double = points.map(_.x).max

    override def minY: Double = points.map(_.y).min

    override def maxY: Double = points.map(_.y).max

    override def minZ: Double = points.map(_.z).min

    override def maxZ: Double = points.map(_.z).max

    override def surfaceArea: Double = ???

    override def volume: Double = ???
  }

  final case class Cuboid(centerX: Double, centerY: Double, centerZ: Double, heightX: Double, heightY: Double, heightZ: Double) extends Shape3D {
    override def x: Double = centerX

    override def y: Double = centerY

    override def z: Double = centerZ

    override def minX: Double = centerX - heightX

    override def maxX: Double = centerX + heightX

    override def minY: Double = centerY - heightY

    override def maxY: Double = centerY + heightY

    override def minZ: Double = centerZ - heightZ

    override def maxZ: Double = centerZ + heightZ

    override def surfaceArea: Double = ???

    override def volume: Double = ???
  }

  final case class Triangle3D(points: Set[Point3D]) extends Shape3D {
    override def x: Double = points.map(_.x).sum / 3

    override def y: Double = points.map(_.y).sum / 3

    override def z: Double = points.map(_.z).sum / 3

    override def minX: Double = points.map(_.x).min

    override def maxX: Double = points.map(_.x).max

    override def minY: Double = points.map(_.y).min

    override def maxY: Double = points.map(_.y).max

    override def minZ: Double = points.map(_.z).min

    override def maxZ: Double = points.map(_.z).max

    override def surfaceArea: Double = ???

    override def volume: Double = ???
  }

}
