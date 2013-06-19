package devteam

import scalikejdbc._
import devteam.misc.DBInitializer

object Database {

  private def useDB[A](func: => A): A = {
    try {
      Class.forName("org.h2.Driver")
      ConnectionPool.singleton("jdbc:h2:file:db/playapp", "sa", "")
      DBInitializer.run()
      func
    } finally {
      ConnectionPool.closeAll()
    }
  }

  def allTableNames(): Array[String] = useDB {
    DB.getAllTableNames().toArray
  }

  def describe(table: String): String = useDB {
    DB.describe(table)
  }

  def columnNames(table: String): Array[String] = useDB {
    DB.getColumnNames(table).toArray
  }

}
