package devteam

import java.sql.ResultSet
import scalikejdbc.TypeBinder

trait FromResultSet[A] {
  def apply(rs: ResultSet): A
}

object FromResultSet {

  implicit class ResultSetOps(val self: ResultSet) extends AnyVal {
    def to[A](implicit A: FromResultSet[A]): A = A.apply(self)
  }

  def apply2[A1, A2, Z](f: (A1, A2) => Z)(a1: String, a2: String)(implicit A1: TypeBinder[A1], A2: TypeBinder[A2]): FromResultSet[Z] =
    new FromResultSet[Z] {
      def apply(rs: ResultSet) =
        f(A1(rs, a1), A2(rs, a2))
    }

  def apply3[A1, A2, A3, Z](f: (A1, A2, A3) => Z)(a1: String, a2: String, a3: String)(implicit A1: TypeBinder[A1], A2: TypeBinder[A2], A3: TypeBinder[A3]): FromResultSet[Z] =
    new FromResultSet[Z] {
      def apply(rs: ResultSet) =
        f(A1(rs, a1), A2(rs, a2), A3(rs, a3))
    }

  def apply4[A1, A2, A3, A4, Z](f: (A1, A2, A3, A4) => Z)(a1: String, a2: String, a3: String, a4: String)(implicit A1: TypeBinder[A1], A2: TypeBinder[A2], A3: TypeBinder[A3], A4: TypeBinder[A4]): FromResultSet[Z] =
    new FromResultSet[Z] {
      def apply(rs: ResultSet) =
        f(A1(rs, a1), A2(rs, a2), A3(rs, a3), A4(rs, a4))
    }
}
