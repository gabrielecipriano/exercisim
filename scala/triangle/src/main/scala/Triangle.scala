case class Triangle(x: Double, y: Double, z: Double) {

  def equilateral: Boolean =
    isATriangle &&
      respectEquilateralDefinition

  def isosceles: Boolean =
    isATriangle &&
      respectScaleneDefinition &&
      respectsTriangleInequality

  def scalene: Boolean =
    isATriangle &&
      !respectEquilateralDefinition &&
      !respectScaleneDefinition &&
      respectsTriangleInequality

  def isATriangle: Boolean = {
    x != 0 || y != 0 || z != 0
  }

  def respectEquilateralDefinition: Boolean = {
    x == y && y == z
  }

  def respectScaleneDefinition: Boolean = {
    x == y || y == z || x == z
  }

  def respectsTriangleInequality: Boolean = {
    y + z >= x &&
      x + z >= y &&
      y + x >= z
  }
}
