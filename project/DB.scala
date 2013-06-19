import sbt._
import sbt.Keys._
import sbt.complete.DefaultParsers._
import sbt.complete.Parser
import sbinary.DefaultProtocol._
import Cache.seqFormat

object Util{

  def invoke[A](loader: ClassLoader, className: String, method: String, params: (Class[_], AnyRef) *): A = {
    invoke(loader.loadClass(className + "$"), method, params: _*)
  }

  def invoke[A](clazz: Class[_], method: String, params: (Class[_], AnyRef) *): A = {
    clazz.getMethod(method, params.map(_._1): _*).invoke(clazz.getField("MODULE$").get(null), params.map(_._2): _*).asInstanceOf[A]
  }

}

trait DBKeys{
  val mainClassLoader = TaskKey[ClassLoader]("main-classloader")

  val describe = InputKey[String]("describe")
  val columnNames = InputKey[Seq[String]]("column-names")
  val tableNames = TaskKey[Seq[String]]("table-names")
}

object DB extends DBKeys{

  private val dbClass = "devteam.Database"

  private def dbParser(tables: Seq[String]): Parser[String] = tables match {
    case Nil => Space ~> any.+.string
    case _   => Space ~> tables.distinct.map(token(_)).reduce(_ | _) | any.+.string
  }

  private def makeTask[A](key: InputKey[A])(func: (String, ClassLoader) => A): Setting[_] = {
    key <<=
      InputTask(Defaults.loadForParser(tableNames)(
        (state, tables) => dbParser(tables.flatten.toList)
      )){ table =>
        (table, mainClassLoader) map func
      }
  }

  private def makeShowTask[A: Manifest](key: InputKey[A]) =
    InputKey[A]("show-" + key.key.label) <<= key.map{x => println(x); x}

  val settings: Seq[Setting[_]] = sbt.seq(
    mainClassLoader <<= (fullClasspath in Compile, scalaInstance).map((path, instance) =>
      classpath.ClasspathUtilities.makeLoader(path.map(_.data), instance)
    ),
    tableNames <<= mainClassLoader.map{ loader =>
      Util.invoke[Array[String]](loader, dbClass, "allTableNames").toSeq
    },
    tableNames <<= tableNames storeAs tableNames,
    makeTask(describe){ (table, loader) =>
      Util.invoke[String](loader, dbClass, "describe", classOf[String] -> table)
    },
    makeTask(columnNames){ (table, loader) =>
      Util.invoke[Array[String]](loader, dbClass, "columnNames", classOf[String] -> table).toSeq
    },
    makeShowTask(describe),
    makeShowTask(columnNames)
  )
}
