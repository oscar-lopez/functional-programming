/*
 * Functional Database
 *
 * Implemented as an in-memory database stored using immutable data structures,
 * supports basic CRUD operations: create table, insert, select, update, delete
 * see test samples at the end, implementing the library case of study.
 */
object Database extends App {

  class DB(private val pName: String) {

    require(pName != null && !pName.isEmpty)

    // each row in the database is represented as
    // a map from column names to field values
    type Record = Map[String, Field[Any]]

    // actual data stored, represented as
    // a map from table names to sets of records
    type Data = Map[String, Set[Record]]

    // result set for queries
    type ResultSet = Set[List[Any]]

    // this describes the database's tables and columns structure
    private var schema: Map[String, Table] = Map[String, Table]()

    // this contains the database's actual information
    private var data: Data = Map[String, Set[Record]]()

    // current table schema being operated upon
    private var currentSchema: Table = _

    // current table information being operated upon
    private var currentInfo: Set[Record] = _

    // current result of the last query
    private var currentResultSet: Set[Record] = _

    /* public API */

    def name: String = pName

    def print(tableName: String): String = {
      data(tableName)
        .toString
        .replace("Set(", s"$name(")
        .replace("Map(", "(")
    }

    // CRUD operations

    def createTable(name: String, columns: Column[Any]*): Table = {
      val newTable = new Table(name)
      newTable.addColumns(columns)
      schema += name -> newTable
      data += name -> Set[Record]()
      newTable
    }

    def from(tableName: String): DB = {
      checkTable(tableName)
      currentSchema = schema(tableName)
      currentInfo = data(tableName)
      this
    }

    def where(conditions: String*): DB = {
      checkStatement()
      currentResultSet = conditions.foldRight(currentInfo)((cond, resultSet) => {
        val (op, exp1, exp2) = parse(cond)
        resultSet.filter(record =>
          apply(op, eval(exp1, record), eval(exp2, record)))
      })
      this
    }

    def insert(assoc: (String, Any)*): Data = {

      checkStatement()
      val values = assoc.toMap
      checkColumns(values.keySet)

      val newRecord = currentSchema.columnsInfo.map(info => {
        val value = values.getOrElse(info.name, info.default)
        info.name -> Field.make(value, info)
      }).toMap

      data += currentSchema.name -> (currentInfo + newRecord)
      reset()
      data

    }

    def select(fields: String*): ResultSet = {

      checkQuery()

      val result =
        if (fields.isEmpty || (fields.length == 1 && fields(0).trim == "*")) {
          currentResultSet.map(record =>
            record.values.map(field => field.value).toList)
        } else {
          checkColumns(fields)
          currentResultSet.map(record =>
            fields.map(field => record(field).value).toList)
        }

      reset()
      result

    }

    def update(assoc: (String, Any)*): Data = {

      checkQuery()
      val values = assoc.toMap
      checkColumns(values.keySet)

      val name = currentSchema.name
      data += name -> currentInfo.diff(currentResultSet)

      val modified = currentResultSet.map(record => {
        currentSchema.columnsInfo.map(info => {
          val value = values.getOrElse(info.name, record(info.name).value)
          info.name -> Field.make(value, info)
        }).toMap
      })

      data += name -> (data(name) ++ modified)
      reset()
      data

    }

    def delete(): Data = {
      checkQuery()
      val name = currentSchema.name
      data += name -> currentInfo.diff(currentResultSet)
      reset()
      data
    }

    override def toString: String = schema.keys.mkString(", ")

    /* private helpers */

    def checkTable(tableName: String): Unit = {
      if (!schema.contains(tableName))
        throw new IllegalArgumentException("Unknown table name")
    }

    def checkStatement(): Unit = {
      if (currentSchema == null || currentInfo == null)
        throw new IllegalArgumentException("Invalid statement")
    }

    def checkQuery(): Unit = {
      if (currentSchema == null || currentInfo == null || currentResultSet == null)
        throw new IllegalArgumentException("Invalid query")
    }

    def checkColumns(columnNames: Iterable[String]): Unit = {
      if (columnNames.toSet.diff(currentSchema.columnNames).nonEmpty)
        throw new IllegalArgumentException("Unknown column name")
    }

    def reset(): Unit = {
      currentSchema = null
      currentInfo = null
      currentResultSet = null
    }

    // conditional expressions evaluator

    private def parse(cond: String): (String, String, String) = {
      try {
        val List(exp1, op, exp2) = "[^\\s']+|'[^']*'".r.findAllIn(cond).toList
        (op, exp1, exp2)
      } catch {
        case _: Exception => throw new IllegalArgumentException("Invalid condition syntax")
      }
    }

    private def eval(exp: String, env: Record): Field[Any] = {

      def is_boolean(x: String): Boolean =
        x == "true" || x == "false"
      def is_int(x: String): Boolean =
        try { x.toInt; true } catch { case _: NumberFormatException => false }
      def is_double(x: String): Boolean =
        try { x.toDouble; true } catch { case _: NumberFormatException => false }
      def is_string(x: String): Boolean =
        x.startsWith("'") && x.endsWith("'")

      if (is_boolean(exp)) {
        Field.make(exp == "true")
      } else if (is_int(exp)) {
        Field.make(exp.toInt)
      } else if (is_double(exp)) {
        Field.make(exp.toDouble)
      } else if (is_string(exp)) {
        Field.make(exp.slice(1, exp.length - 1))
      } else {
        try {
          env(exp)
        } catch {
          case _: Exception =>
            throw new IllegalArgumentException("Unknown field")
        }
      }

    }

    private def apply(op: String, exp1: Field[Any], exp2: Field[Any]): Boolean = {
      op match {
        case "==" | "="  => exp1.compare(exp2) == 0
        case "!=" | "<>" => exp1.compare(exp2) != 0
        case ">=" => exp1 >= exp2
        case "<=" => exp1 <= exp2
        case ">"  => exp1 >  exp2
        case "<"  => exp1 <  exp2
        case _ => throw new IllegalArgumentException("Unknown operator")
      }
    }

  }

  class Table(private val pName: String) {

    require(pName != null && !pName.isEmpty)
    private var columns: Map[String, Column[Any]] = Map[String, Column[Any]]()

    def name: String = pName
    def columnNames: Set[String] = columns.keySet
    def columnsInfo: Iterable[Column[Any]] = columns.values

    def addColumns(cols: Seq[Column[Any]]): Unit = {

      // check that there are no duplicate columns
      val names = cols.map(col => col.name)
      if (names.size != names.toSet.size)
        throw new IllegalArgumentException("Duplicate column names")

      // populate columns map
      columns = cols.foldLeft(columns)((acc, col) =>
        acc + (col.name -> col))

    }

    override def toString: String = {
      name + "\n" +
        (for ((k, v) <- columns) yield { s"$k: ${v.valueType} (${v.default})" })
          .mkString(" | ")
    }

  }

  // schema columns

  case class Column[+T](name: String, default: T) {

    require(name != null && !name.isEmpty)

    def valueType: String = {
      val name = default.getClass.toString
      val idx  = name.lastIndexOf('.')
      if (idx == -1) name; else name.substring(idx + 1)
    }

    override def toString: String = s"Column[$valueType]($name, $default)"

  }

  // actual fields

  abstract class Field[+T](private val pValue: T, info: Column[T])
    extends Ordered[Field[Any]] {

    def value: T = pValue
    def valueType: String = info.valueType
    override def toString: String = value.toString

  }

  object Field {

    def make[T](value: T, col: Column[T] = null): Field[Any] = {
      value match {
        case v: Boolean =>
          if (col != null)
            BooleanField(v, col.asInstanceOf[Column[Boolean]])
          else
            new BooleanField(v)
        case v: Int =>
          if (col != null)
            IntField(v, col.asInstanceOf[Column[Int]])
          else
            new IntField(v)
        case v: Double =>
          if (col != null)
            DoubleField(v, col.asInstanceOf[Column[Double]])
          else
            new DoubleField(v)
        case v: String =>
          if (col != null)
            StringField(v, col.asInstanceOf[Column[String]])
          else
            new StringField(v)
        case _ =>
          throw new IllegalArgumentException("Unknown field type")
      }
    }

  }

  case class BooleanField(private val pValue: Boolean, info: Column[Boolean])
    extends Field[Boolean](pValue, info) {

    def this(pValue: Boolean) = {
      this(pValue, Column[Boolean]("dummy", false))
    }

    override def compare(that: Field[Any]): Int = {
      that.valueType match {
        case "Boolean" =>
          value.compareTo(that.asInstanceOf[Field[Boolean]].value)
        case _ =>
          throw new IllegalArgumentException("Invalid comparison with boolean")
      }
    }

  }

  case class IntField(private val pValue: Int, info: Column[Int])
    extends Field[Int](pValue, info) {

    def this(pValue: Int) = {
      this(pValue, Column[Int]("dummy", 0))
    }

    override def compare(that: Field[Any]): Int = {
      that.valueType match {
        case "Integer" =>
          value.compareTo(that.asInstanceOf[Field[Integer]].value)
        case "Double" =>
          value.toDouble.compareTo(that.asInstanceOf[Field[Double]].value)
        case _ =>
          throw new IllegalArgumentException("Invalid comparison with integer")
      }
    }

  }

  case class DoubleField(private val pValue: Double, info: Column[Double])
    extends Field[Double](pValue, info) {

    def this(pValue: Double) = {
      this(pValue, Column[Double]("dummy", 0.0))
    }

    override def compare(that: Field[Any]): Int = {
      that.valueType match {
        case "Integer" =>
          value.compareTo(that.asInstanceOf[Field[Integer]].value.toDouble)
        case "Double" =>
          value.compareTo(that.asInstanceOf[Field[Double]].value)
        case _ =>
          throw new IllegalArgumentException("Invalid comparison with double")
      }
    }

  }

  case class StringField(private val pValue: String, info: Column[String])
    extends Field[String](pValue, info) {

    def this(pValue: String) = {
      this(pValue, Column[String]("dummy", ""))
    }

    override def compare(that: Field[Any]): Int = {
      that.valueType match {
        case "String" =>
          value.compareTo(that.asInstanceOf[Field[String]].value)
        case _ =>
          throw new IllegalArgumentException("Invalid comparison with string")
      }
    }

  }

  /* tests */

  // sample tables from case of study

  val db = new DB("db_library")

  db.createTable("books",
    Column[Int]("id", default = 1),
    Column[Boolean]("is_electronic", default = false),
    Column[String]("author", default = ""),
    Column[String]("title", default = ""),
    Column[Int]("pages", default = 0),
    Column[Int]("number_times_borrowed", default = 0), // for physical items only
    Column[Int]("day_out", default = 0), // for physical items only, timestamp in secs
    Column[Int]("count_internal_accesses", default = 0), // for electronic items only
    Column[Int]("count_external_accesses", default = 0), // for electronic items only
    Column[Double]("external_access_cost", default = 1000), // for electronic items only
    Column[Double]("profit_value", default = 0.0)) // for electronic items only

  db.createTable("magazines",
    Column[Int]("id", default = 1),
    Column[Boolean]("is_electronic", default = false),
    Column[String]("author", default = ""),
    Column[String]("title", default = ""),
    Column[Int]("pages", default = 0),
    Column[Int]("issue_number", default = 0), // for magazines only
    Column[Int]("number_times_borrowed", default = 0), // for physical items only
    Column[Int]("day_out", default = 0), // for physical items only, timestamp in secs
    Column[Int]("count_internal_accesses", default = 0), // for electronic items only
    Column[Int]("count_external_accesses", default = 0), // for electronic items only
    Column[Double]("external_access_cost", default = 1000), // for electronic items only
    Column[Double]("profit_value", default = 0.0)) // for electronic items only

  // sample inserts

  db.from("books")
    .insert("id" -> 1, "author" -> "Larry Niven", "title" -> "Ring World", "pages" -> 300)
  db.from("books")
    .insert("id" -> 2, "author" -> "Isaac Asimov", "title" -> "Foundation", "pages" -> 250)
  db.from("books")
    .insert("id" -> 3, "author" -> "Arthur Clarke", "title" -> "2001", "pages" -> 400)
  db.from("books")
    .insert("id" -> 2, "author" -> "Isaac Asimov", "title" -> "I, Robot", "pages" -> 280)

  println(db.print("books"))

  // sample selects

  var rs: Set[List[Any]] = _

  rs = db.from("books")
         .where("author = 'Larry Niven'", "pages >= 300") // AND of multiple conditions
         .select("id", "title") // individual columns can be selected

  println(rs)

  rs = db.from("books")
         .where("author = 'Isaac Asimov'") // multiple results
         .select("*") // select all columns

  println(rs)

  // sample updates

  db.from("books")
    .where("title = '2001'") // multiple records can be updated
    .update("title" -> "2001: A Space Odyssey", "pages" -> 450) // multiple fields can be updated

  println(db.from("books").where("id = 3").select("title", "pages"))

  // sample deletes

  db.from("books")
    .where("author <> 'Arthur Clarke'") // multiple records can deleted
    .delete()

  println(db.print("books"))

}
