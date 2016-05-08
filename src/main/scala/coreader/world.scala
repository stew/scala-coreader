package coreader

import cats._

/**
 * A trait that allows me to interact with the world
 */
trait World[A] {
  def extract: A
  def console: Console[A]

  def map[B](f: A => B): World[B]
  def coflatMap[B](f: World[A] => B): World[B]
}

/**
 * A World implementation that actually interacts with the real world
 */
case class RealWorld[A](extract: A) extends World[A] {
  override def console: Console[A] = RealConsole(extract)

  override def map[B](f: A => B): RealWorld[B] =
    RealWorld(f(extract))

  override def coflatMap[B](f: World[A] => B): RealWorld[B] =
    RealWorld(f(this))
}

object RealWorld {
  def bigBang = RealWorld(())
}

object World {

  implicit val worldInstances: Comonad[World] = new Comonad[World] {
    override def extract[A](fa: World[A]): A = fa.extract
    override def map[A,B](fa: World[A])(f: A => B): World[B] = fa map f
    override def coflatMap[A,B](fa: World[A])(f: World[A] => B): World[B] = fa coflatMap f
  }
}
