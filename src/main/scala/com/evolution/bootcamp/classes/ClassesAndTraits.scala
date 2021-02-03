package com.evolution.bootcamp.classes

// Homework
//
// Add additional 2D shapes such as triangle and square.
//
// In addition to the 2D shapes classes, add also 3D shapes classes
// (origin, point, sphere, cube, cuboid, 3D triangle - you can add
// others if you think they are a good fit).
//
// Add method `area` to 2D shapes.
//
// Add methods `surfaceArea` and `volume` to 3D shapes.
//
// If some of the implementation involves advanced math, it is OK
// to skip it (leave unimplemented), the primary intent of this
// exercise is modelling using case classes and traits, and not math.
object ClassesAndTraits {

  sealed trait Shape extends Located with Bounded with Area

  sealed trait Located {
    def x: Double

    def y: Double
  }

  sealed trait Bounded {
    def minX: Double

    def maxX: Double

    def minY: Double

    def maxY: Double
  }

  sealed trait Area {
    def area: Double
  }

  final case class Point(x: Double, y: Double) extends Shape {
    override def area: Double = 0

    override def minX: Double = x

    override def maxX: Double = x

    override def minY: Double = y

    override def maxY: Double = y
  }

  final case class Circle(centerX: Double, centerY: Double, radius: Double) extends Shape {
    override def area: Double = Math.PI * Math.pow(radius, 2)

    override def x: Double = centerX

    override def y: Double = centerY

    override def minX: Double = centerX - radius

    override def maxX: Double = centerX + radius

    override def minY: Double = centerY - radius

    override def maxY: Double = centerY + radius
  }

  sealed abstract case class Triangle private(p1: Point, p2: Point, p3: Point) extends Shape {
    val vertices = Seq(p1, p2, p3)

    override def x: Double = vertices.map(_.x / 3).sum

    override def y: Double = vertices.map(_.y / 3).sum

    override def minX: Double = vertices.map(_.x).min

    override def maxX: Double = vertices.map(_.x).max

    override def minY: Double = vertices.map(_.y).min

    override def maxY: Double = vertices.map(_.y).max
  }

  object Triangle {
    def sideLength(p1: Point, p2: Point): Double = {
      Math.sqrt(Math.pow(p2.x - p1.x, 2) + Math.pow(p2.y - p1.y, 2))
    }

    def apply(p1: Point, p2: Point, p3: Point): Option[Triangle] = {
      val side1 = sideLength(p1, p2)
      val side2 = sideLength(p2, p3)
      val side3 = sideLength(p3, p1)
      val halfPerimeter = (side1 + side2 + side3) / 2

      if (side1 + side2 < side3 || side1 + side3 < side2 || side2 + side3 < side1) None
      else Some(new Triangle(p1, p2, p3) {
        override def area: Double = Math.sqrt(halfPerimeter * (halfPerimeter - side1) * (halfPerimeter - side2) * (halfPerimeter - side3))
      })
    }
  }

  sealed abstract case class Square private(p1: Point, p2: Point, p3: Point, p4: Point) extends Shape {
    override def area: Double = ???

    override def x: Double = ???

    override def y: Double = ???

    override def minX: Double = ???

    override def maxX: Double = ???

    override def minY: Double = ???

    override def maxY: Double = ???
  }

  object Square {
    def sideLength(p1: Point, p2: Point): Double = {
      Math.sqrt(Math.pow(p2.x - p1.x, 2) + Math.pow(p2.y - p1.y, 2))
    }

    def apply(p1: Point, p2: Point, p3: Point, p4: Point): Option[Square] = {
      val side1 = sideLength(p1, p2)
      val side2 = sideLength(p2, p3)
      val side3 = sideLength(p3, p4)
      val side4 = sideLength(p4, p1)

      if (side1 == side2 && side2 == side3 && side3 == side4) Some(new Square(p1, p2, p3, p4) {})
      else None
    }
  }

  sealed trait Shape3D extends Located3D with Bounded3D with SurfaceArea with Volume

  sealed trait Located3D extends Located {
    def z: Double
  }

  sealed trait Bounded3D extends Bounded {
    def minZ: Double

    def maxZ: Double
  }

  sealed trait SurfaceArea {
    def surfaceArea: Double
  }

  sealed trait Volume {
    def volume: Double
  }

  case class Origin3D(p: Point3D) extends Located3D {
    override def z: Double = p.z

    override def x: Double = p.x

    override def y: Double = p.y
  }

  case class Point3D(x: Double, y: Double, z: Double) extends Shape3D {
    override def minX: Double = x

    override def maxX: Double = x

    override def minY: Double = y

    override def maxY: Double = y

    override def volume: Double = 0

    override def minZ: Double = z

    override def maxZ: Double = z

    override def surfaceArea: Double = 0

    def distanceTo(other: Point3D): Double =
      Math.sqrt(Math.pow(x - other.x, 2) + Math.pow(y - other.y, 2) + Math.pow(z - other.z, 2))
  }

  case class Sphere(center: Point3D, radius: Double) extends Shape3D {
    override def x: Double = center.x

    override def y: Double = center.y

    override def z: Double = center.z

    override def minX: Double = center.x - radius

    override def maxX: Double = center.x + radius

    override def minY: Double = center.y - radius

    override def maxY: Double = center.y + radius
    override def minZ: Double = center.z - radius

    override def maxZ: Double = center.z + radius

    override def volume: Double = ???

    override def surfaceArea: Double = ???
  }

  /**
   * can be constructed by 4 points that forms a corner
   * so we know width, length and height since it has right angles
   *
   * @param p1 point of the first vertex
   * @param p2 point of the second vertex so p1 and p2 forms an edge
   * @param p3 point of the third vertex so p1 and p3 forms an edge
   * @param p4 point of the fourth vertex so p1 and p4 forms an edge
   */
  case class Cuboid(p1: Point3D, p2: Point3D, p3: Point3D, p4: Point3D) extends Shape3D {
    //todo extract smart constructor to check the angles
    override def x: Double = ???

    override def y: Double = ???

    override def minX: Double = ???

    override def maxX: Double = ???

    override def minY: Double = ???

    override def maxY: Double = ???

    override def volume: Double = ???

    override def z: Double = ???

    override def minZ: Double = ???

    override def maxZ: Double = ???

    override def surfaceArea: Double = ???
  }

  //extends case class since cuboid equals, hashcode, toString methods can be applied to the cube
  sealed abstract class Cube private(p1: Point3D, p2: Point3D, p3: Point3D, p4: Point3D) extends Cuboid(p1, p2, p3, p4)
  object Cube {
    def apply(p1: Point3D, p2: Point3D, p3: Point3D, p4: Point3D): Option[Cube] = {
      //todo check the angles
      (p1 distanceTo p2, p1 distanceTo p3, p1 distanceTo p4) match {
        case (l, h, w) if (l ==h && w == l) => Some(new Cube(p1, p2, p3, p4){})
        case _ => None
      }
    }
  }

  case class Triangle3D(p1: Point3D, p2: Point3D, p3: Point3D) extends Shape3D {
    override def z: Double = ???

    override def minZ: Double = ???

    override def maxZ: Double = ???

    override def surfaceArea: Double = ???

    override def minX: Double = ???

    override def maxX: Double = ???

    override def minY: Double = ???

    override def maxY: Double = ???

    override def volume: Double = ???

    override def x: Double = ???

    override def y: Double = ???
  }

}
