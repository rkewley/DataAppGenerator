package tools

import com.mysql.jdbc._
import javax.sql._

class DB {
import java.sql.{Connection, DriverManager, ResultSet};

  // Change to Your Database Config
  val conn_str = "jdbc:mysql://localhost:3306/SECourseData"

  // Load the driver
  classOf[com.mysql.jdbc.Driver]

  // Setup the connection
  val conn = DriverManager.getConnection(conn_str, "student", "student")
  val metaData = conn.getMetaData();


  
  def getMetaDataFields(tableName: String, rs: ResultSet): List[FieldMetaData] = {
    def addMetaData(list: List[FieldMetaData]): List[FieldMetaData] = {
      if (rs.next) addMetaData(list ::: List(FieldMetaData.newFieldMetaData(tableName, rs))) else list
    }
    addMetaData(List[FieldMetaData]())
  }
  
  def getMetaDataFields(tableName: String): List[FieldMetaData] = {
    val rs = metaData.getColumns(null, null, tableName, null)
    val fields = getMetaDataFields(tableName, rs)
    rs.close
    fields
  }
  
  def getFieldList(tableName: String): List[FieldMetaData] = {
    val statement = conn.createStatement(ResultSet.TYPE_FORWARD_ONLY, ResultSet.CONCUR_READ_ONLY)
    val rs = statement.executeQuery("SELECT * FROM `" + tableName + "`")
    val md = rs.getMetaData()

    def getFieldMetaData(i: Int, list: List[FieldMetaData]): List[FieldMetaData] = {
      if (i > md.getColumnCount) list else {
        val fieldMetaData = new FieldMetaData(tableName, md.getColumnName(i), md.getColumnClassName(i), md.getColumnTypeName(i), md.isAutoIncrement(i))
        getFieldMetaData(i+1, list ::: List(fieldMetaData))
      }
    }
    val fields = getFieldMetaData(1, List[FieldMetaData]())
    statement.close
    rs.close
    return fields
  }

  def getPrimaryKeys(tableName: String): List[FieldMetaData] = {
    val rs = metaData.getPrimaryKeys(null, null, tableName: String)
    def getPrimaryKeysMetaData(list: List[FieldMetaData]): List[FieldMetaData] = {
      if(rs.next) {
        val column = rs.getString("COLUMN_NAME")
        val rs2 = metaData.getColumns(null, null, tableName, column)
        rs2.next
        val sqlType = rs2.getString("TYPE_NAME")
        val clazz = FieldMetaData.clazzMapping(sqlType)
        val isAutoIncrement = rs2.getString("IS_AUTOINCREMENT") match {
	      case "YES" => true
	      case (_) => false
	    }
        rs2.close
        getPrimaryKeysMetaData(list ::: List(new FieldMetaData(tableName, column, clazz, sqlType, isAutoIncrement)))
      }
        else list
    }  
    val primaryKeys = getPrimaryKeysMetaData(List[FieldMetaData]())
    rs.close
    return primaryKeys
  }
  
  def getForeignKeys(tableName: String): List[ForeignKeyMetaData] = {
    val rs = metaData.getImportedKeys(null, null, tableName: String)
    def getForeignKeysMetaData(list: List[ForeignKeyMetaData]): List[ForeignKeyMetaData] = {
      if(rs.next) {
        val fkColumn = rs.getString("FKCOLUMN_NAME")
        val rs2 = metaData.getColumns(null, null, tableName, fkColumn)
        rs2.next
        val sqlType = rs2.getString("TYPE_NAME")
        val fkClazz = FieldMetaData.clazzMapping(sqlType)
        val isAutoIncrement = rs2.getString("IS_AUTOINCREMENT") match {
	      case "YES" => true
	      case (_) => false
	    }
        
        rs2.close
        val fk = new FieldMetaData(tableName, fkColumn, fkClazz, sqlType, isAutoIncrement)
        
        val pkTable = rs.getString("PKTABLE_NAME")
        val pkColumn = rs.getString("PKCOLUMN_NAME")
        val rs3 = metaData.getColumns(null, null, pkTable, pkColumn)
        rs3.next
        val pkSqlType = rs3.getString("TYPE_NAME")
        val pkClazz = FieldMetaData.clazzMapping(pkSqlType)
        val pkIsAutoIncrement = rs3.getString("IS_AUTOINCREMENT") match {
	      case "YES" => true
	      case (_) => false
	    }

        rs3.close
        val pk = new FieldMetaData(pkTable, pkColumn, pkClazz, pkSqlType, pkIsAutoIncrement)
        val foreignKey = new ForeignKeyMetaData(fk, pk)
        getForeignKeysMetaData(list ::: List(new ForeignKeyMetaData(fk, pk)))
      }
        else list
    }  
    getForeignKeysMetaData(List[ForeignKeyMetaData]())
  }
  
  def isKey(field: FieldMetaData, keys: List[FieldMetaData]):Boolean = {
    val keyList = keys.map(key => key.column)
    println("Checking if : " + keyList.toString + " contains " + field.column)
    println(keyList.contains(field.column))
    keyList.contains(field.column)
  }
  
  def isPrimaryKey(tableName: String, field: FieldMetaData): Boolean = {
    isKey(field, getPrimaryKeys(tableName))
  }
  
  def getKeyMetaData(field: FieldMetaData, foreignKeys: List[ForeignKeyMetaData]): ForeignKeyMetaData = {
    foreignKeys.find(fkMetaData => fkMetaData.fk.column == field.column).get
  }
  
  def isForeignKey(tableName: String, field: FieldMetaData): Boolean = {
    isKey(field, getForeignKeys(tableName).map(foreignKey => foreignKey.fk))
  }
  
}

object Main {
  def main(args: Array[String]) {
    val database = new tools.DB
    println(database.metaData.getCatalogTerm)
    val rs = database.metaData.getCatalogs
    while(rs.next) println (rs.getString("TABLE_CAT"))
    database.getPrimaryKeys("Courses").foreach(field => println(field.column + ", " + field.clazz))
    val fks = database.getForeignKeys("Courses")
    fks.foreach( foreignKey => {
      println("Foreign Key: " + foreignKey.fk.toString)
      println("Primary Key: " + foreignKey.pk.toString)
    })
    ModelGenerator.writeModel("Courses", database)
    println("Hello")
  }
  
}
