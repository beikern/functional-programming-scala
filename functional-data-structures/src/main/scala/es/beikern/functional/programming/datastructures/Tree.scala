package es.beikern.functional.programming.datastructures

sealed trait Tree[+A] {

  /*
   * Write a function size that counts the number of nodes (leaves and branches) in a tree
   */
  def size: Int = {
    def go(t: Tree[A]): Int = {
      t match {
        case Leaf(_)             => 1
        case Branch(left, right) => 1 + go(left) + go(right)
      }
    }
    go(this)
  }

  /*
   * Write a function maximum that returns the maximum element in a Tree[Int]. (Note: In Scala, you can use x.max(y) or x max y
   * to compute the maximum of a two integers x and y)
   */
  def maximum[B >: A](implicit num: Numeric[B]): B = {
    def go(tree: Tree[B]): B = {
      tree match {
        case Leaf(v)      => v
        case Branch(l, r) => num.max(go(l), go(r))
      }
    }
    go(this)
  }

  /*
   * Write a function depth that returns the maximum path length from the root of a tree to a any leaf.
   */
  def depth: Int = {
    def go(tree: Tree[A]): Int = {
      tree match {
        case Branch(l, r) => (1 + go(l)).max(1 + go(r))
        case Leaf(_)      => 1
      }
    }
    go(this)
  }

  /*
   * Write a function map, analogous to the method of the same name on List, that modifies each element in a tree with a given function
   */

  def map[B](f: A => B): Tree[B] = {
    def mapGo(t: Tree[A]): Tree[B] = {
      t match {
        case Branch(l, r) => Branch(mapGo(l), mapGo(r))
        case Leaf(l)      => Leaf(f(l))
      }
    }
    mapGo(this)
  }

  /*
   * Generalize size, maximum, depth and map writing a new function fold that abstracts over their similarities. Reimplement them in
   * terms of this more general function. Can you draw an analogy between this fold function and the left and right folds for List?
   */
  def fold[B](z: B)(f: (A, B) => B): B = {
    def go(t: Tree[A], acc: B): B = {
      t match {
        case Leaf(l)      => f(l, acc)
        case Branch(l, r) => go(l, go(r, acc))
      }
    }
    go(this, z)
  }

  def sizeFold: Int = {
    fold(0)((_, acc) => 1 + acc)
  }

}
case class Leaf[A](value: A)                        extends Tree[A]
case class Branch[A](left: Tree[A], right: Tree[A]) extends Tree[A]

object Tree {
  def apply[A](t: Tree[A]): Tree[A] = t
}
