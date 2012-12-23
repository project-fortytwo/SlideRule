package com.fortytwo.sliderule.vector

trait Vector {
  val projections: Array[Real]
  def +(rhs: Vector): Vector
  def -(rhs: Vector): Vector
  def *(rhs: Real): Vector
  def /(rhs: Real): Vector
  def *:(lhs: Real): Vector = this * lhs
  def /:(lhs: Real): Vector = this / lhs
  def -:(): Vector = Vector(for(x <- projections) yield (-x)) 
  def dot(rhs: Vector): Real
  def magnitude(): Real = math.sqrt(this.dot(this))
  def unit(): Vector = this / magnitude
}

object Vector {
  def apply(arg0: Array[Real]): Vector = {
    require(arg0.length > 1, "Array of size " + arg0.length + " fails to create Vector")
    if(arg0.length == 2) {new Vector2(arg0(0),arg0(1))}
    if(arg0.length == 3) {new Vector3(arg0(0),arg0(1),arg0(2))}
    new VectorN(arg0)
  }
  def unapply(arg0: Array[Real]): Option[Array[Real]] = Some(arg0)
}

class VectorN(override val projections: Array[Real]) extends Vector {
  override def +(rhs: Vector): Vector = Vector(for(x <- projections.zip(rhs.projections)) yield(x._1 + x._2))
  override def -(rhs: Vector): Vector = Vector(for(x <- projections.zip(rhs.projections)) yield(x._1 - x._2))
  override def *(rhs: Real): Vector = Vector(for(x <- projections) yield rhs * x)
  override def /(rhs: Real): Vector = {val inv_rhs = 1/rhs; Vector(for(x <- projections) yield inv_rhs * x)}
  override def dot(rhs: Vector): Real = (for(x <- projections.zip(rhs.projections)) yield(x._1 * x._2)).sum
  
  override def equals(that:Any): Boolean = {
    that match {
      case that:Vector => {
        (projections.length == projections.length) &&
        (projections.zip(that.projections)).forall(x => (x._1 == x._2))
      }
      case _ => false
    }
  }
  
  override def hashCode(): Int = {
    projections.sum.toInt
  }
}

class Vector2(private val x:Real, private val y:Real) extends Vector {
  override val projections: Array[Real] = Array(x,y)
  
  override def +(rhs: Vector): Vector = {
    rhs match {
      case that:Vector2 => new Vector2(x + that.x, y + that.y)
      case that => throw new UnsupportedOperationException("Vector2 fails to combine with Vector" + that.projections.length)
    }
  }
  
  override def -(rhs: Vector): Vector = {
    rhs match {
      case that:Vector2 => new Vector2(x - that.x, y - that.y)
      case that => throw new UnsupportedOperationException("Vector2 fails to combine with Vector" + that.projections.length)
    }
  }
  
  override def *(rhs: Real): Vector = new Vector2(x * rhs, y * rhs)
  override def /(rhs: Real): Vector = new Vector2(x / rhs, x / rhs)
  
  override def dot(rhs: Vector): Real = {
    rhs match {
      case that:Vector2 => (x * that.x) + (y * that.y)
      case that => throw new UnsupportedOperationException("Vector2 fails to combine with Vector" + that.projections.length)
    }
  }
  
  override def equals(that: Any): Boolean = {
    that match {
      case that:Vector2 => (x == that.x && y == that.y)
      case _ => false
    }
  }
  
  override def hashCode(): Int = (x + y).toInt
}

object Vector2 {
  def apply(x:Real, y:Real): Vector2 = new Vector2(x,y)
  def unapply(x:Real, y:Real): Option[(Real,Real)] = Some(x,y)
}

class Vector3(private val x:Real, private val y:Real, private val z:Real) extends Vector {
  override val projections: Array[Real] = Array(x,y,z)
  override def +(rhs: Vector): Vector = {
    rhs match {
      case that:Vector3 => new Vector3(x + that.x, y + that.y, z + that.z)
      case that => throw new UnsupportedOperationException("Vector3 fails to combine with Vector" + that.projections.length)
    }
  }
  override def -(rhs: Vector): Vector = {
    rhs match {
      case that:Vector3 => new Vector3(x - that.x, y - that.y, z - that.z)
      case that => throw new UnsupportedOperationException("Vector3 fails to combine with Vector" + that.projections.length)
    }
  }
  override def *(rhs: Real): Vector = new Vector3(x * rhs, y * rhs, z * rhs)
  override def /(rhs: Real): Vector = new Vector3(x / rhs, y / rhs, z / rhs)
  override def *:(lhs: Real): Vector = this * lhs
  override def /:(lhs: Real): Vector = this / lhs
  override def dot(rhs: Vector): Real  = {
    rhs match {
      case that:Vector3 => (x * that.x) + (y * that.y) + (z * that.z)
      case that => throw new UnsupportedOperationException("Vector3 fails to combine with Vector" + that.projections.length)
    }
  }
  override def magnitude(): Real = math.sqrt(x*x + y*y + z*z)
  override def unit(): Vector = this / magnitude
  override def equals(that: Any): Boolean = {
    that match {
      case that:Vector3 => (x == that.x && y == that.y && z == that.z)
      case _ => false
    }
  }
  
  override def hashCode(): Int = (x + y + z).toInt
}

object Vector3 {
  def apply(x:Real, y:Real, z:Real): Vector3 = new Vector3(x,y,z)
  def unapply(x:Real, y:Real, z:Real): Option[(Real,Real,Real)] = Some(x,y,z)
}