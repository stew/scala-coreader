import cats.data._
import cats._

package object coreader {
  implicit class CokleisliOps[F[_],A,B](ck: Cokleisli[F,A,B]) {
    def local[G[_]](implicit nt: G ~> F): Cokleisli[G,A,B] = Cokleisli(g => ck.run(nt(g)))
  }
}
