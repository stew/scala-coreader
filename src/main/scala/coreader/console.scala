package coreader

import cats._
import cats.data._
import cats.arrow._

/**
 * This is (uh, I think?) a coalgebra represending read and write
 * operations to a console.
 */
trait Console[A] {
  // this is the comonadic copoint
  def extract: A

  // write a string to the console
  def write: Unit

  // read a string from the user
  def read: String

  // Console is Functorial
  def map[B](f: A => B): Console[B]

  // Console is Comonadic
  def coflatMap[B](f: Console[A] => B): Console[B]
}

/**
 * An implementation of console which actually interacts with the
 * console, side-effecting.
 */
case class RealConsole[A](extract: A) extends Console[A] {
  override def write: Unit = println(extract)
  override def read: String = scala.Console.readLine

  override def map[B](f: A => B): RealConsole[B] =
    RealConsole(f(extract))

  override def coflatMap[B](f: Console[A] => B): RealConsole[B] =
    RealConsole(f(this))
}

object Console {
  type ConsoleApp[A,B] = Cokleisli[Console, A, B]

  /** read the A value */
  def extract[A]: Cokleisli[Console, A, A] =
    Cokleisli(_.extract)

  def write: ConsoleApp[String,Unit] = Cokleisli(_.write)
  def read: ConsoleApp[Unit,String] = Cokleisli(_.read)

  implicit val consoleInstances: Comonad[Console] = new Comonad[Console] {
    override def extract[A](fa: Console[A]): A = fa.extract
    override def map[A,B](fa: Console[A])(f: A => B): Console[B] = fa map f
    override def coflatMap[A,B](fa: Console[A])(f: Console[A] => B): Console[B] = fa coflatMap f
  }
  
  implicit val fromWorld: World ~> Console = new NaturalTransformation[World,Console] {
    def apply[A](w: World[A]): Console[A] = w.console
  }
}

