package coreader

import cats._
import cats.data._
import cats.implicits._

object App {
  /** A path from A to B in some World */
  type App[A,B] = Cokleisli[World, A, B]

  /** read the A value */
  def extract[A]: App[A,A] =
    Cokleisli(_.extract)
}

object Main {
  import App._

  val askName: App[Unit, String] = 
    (Cokleisli.pure("what is your name") andThen Console.write andThen Console.read).local

  val sayHello: App[String, Unit] =
    (Console.extract andThen Console.write.lmap[String]("Hello, " + _)).local

  /**
   * Ask the user their name, then give them a personalized greeting
   */
  val app = (askName andThen sayHello)

  def main(argv: Array[String]): Unit = {
    app.run(RealWorld.bigBang)
  }
}
