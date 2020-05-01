package org.unsupervise.instance

import _root_.scala.language.higherKinds
import _root_.scala.collection.{immutable, mutable, GenSeq}
import _root_.scala.collection.parallel.mutable.ParArray
import _root_.scala.language.implicitConversions
import _root_.scala.reflect.ClassTag
import _root_.scala.language.postfixOps
import _root_.scala.language.existentials
import org.unsupervise.collections._
import org.unsupervise._
import org.unsupervise.persistance._
import org.unsupervise.enums._
import org.unsupervise.collections.CollectionsHandlersImplicits._
import org.unsupervise.conversions._
import _root_.scala.reflect.runtime.universe._
/**
 *
 */
trait InstanceBuilder[Inst[A] <: Instance[A]] {

  def build[CC[_], T](coll: CC[T]): Inst[T]

}
/**
 *
 */
trait InstanceOps[T, +InstOut[X] <: Instance[X]] extends InstanceT[T] with InstanceTOps[T, InstOut]
/**
 *
 */
trait Instance[T] extends InstanceT[T] with InstanceOps[T, Instance] {
  /**
   *
   */
  final def toInst[Inst[A] <: Instance[A]](cv: Instance[T] => Inst[T]): Inst[T] = cv(this)
  /**
   *
   */
  final def instCast[Inst[_] <: Instance[_]]: Inst[T] = this.asInstanceOf[Inst[T]]
  /**
   *
   */
  private[unsupervise] final def collCast[CC2[_]]: CC2[T] = collection.asInstanceOf[CC2[T]]

}
/**
 *
 */
object Instance {
  /**
   *
   */
  def empty[T : ClassTag, Inst[A] <: Instance[A]](implicit builder: EmptyableInst[Inst]): Inst[T] = builder.empty[T]
  /**
   *
   */
  def apply[T, CC[_], Inst[A] <: Instance[A]](cc: CC[T])(implicit cv: CC[T] => Inst[T], ct: ClassTag[T]): Inst[T] = cv(cc)
  /**
   *
   */
  def union[T, Inst[A] <: Instance[A]](instance1: Inst[T], instance2: Instance[T]): Inst[T] = {
    (instance1 v instance2).instCast[Inst]
  }
  /**
   *
   */
  def v[T, Inst[A] <: Instance[A]](instance1: Inst[T], instance2: Instance[T]): Inst[T] = union(instance1, instance2)
  /**
   *
   */
  def intersect[T, Inst[A] <: Instance[A]](instance1: Inst[T], instance2: Instance[T]): Inst[T] = {
    (instance1 v instance2).instCast[Inst]
  }
  /**
   *
   */
  def ^[T, Inst[A] <: Instance[A]](instance1: Inst[T], instance2: Instance[T]): Inst[T] = intersect(instance1, instance2)
  /**
   *
   */
  def zip[T, U : ClassTag, Inst[A] <: Instance[A]](instance1: Inst[T], instance2: Instance[U]): Inst[(T, U)] = {
    instance1.zip(instance2).instCast[Inst]
  }

  // def extractInstanceNature(seq: Seq[Instance[_]]): Set[InstanceEnum] = seq.map(_.nature).toSet

}
/**
 *
 */
trait EmptyableColl[CC[_]] {
  /**
   *
   */
  def empty[T : ClassTag]: CC[T] 
}
/**
 *
 */
trait EmptyableInst[Inst[A] <: Instance[A]] {
  /**
   *
   */
  def empty[T : ClassTag]: Inst[T]
}
/**
 *
 */
trait IdentifiableInstance[Inst[A] <: Instance[A]] {
  /**
   *
   */
  def identifiers[R](i: Inst[R]): Inst[(R, Long)]

}
/**
 *
 */
object BasicIdentifiableInstance extends IdentifiableInstance[Instance] {

  def identifiers[R](i: Instance[R]): Instance[(R, Long)] = i.zipWithIndexL

}