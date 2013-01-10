package tools

import java.sql.ResultSet
/*
class FieldMetaData(rs: ResultSet) {
	val column = rs.getString("COLUMN_NAME")
	val clazz = rs.getString("TYPE_NAME")
}*/

class FieldMetaData(val table: String, val column: String, val clazz: String, val sqlType: String, val isAutoIncrement: Boolean) {
  println("Create new FieldMetaData: " + toString)
  override def toString = table + ", " + column + ", " + clazz + ", " + isAutoIncrement
}

object FieldMetaData {
  
  def clazzMapping(sqlType: String) = {
    sqlType match {
      case "VARCHAR" => "java.lang.String"
      case "TEXT" => "java.lang.String"
      case "LONGTEXT" => "java.lang.String"
      case "INT" => "java.lang.Long"
      case "DOUBLE" => "java.lang.Double"
      case "BIT" => "java.lang.Boolean"
      case "BOOLEAN" => "java.lang.Boolean"
      case "TINYINT" => "java.lang.Boolean"
      case(_) => 
        println("Type Not Found")
        "Type Not Found"
    }
  }
  
  def newFieldMetaData(tableName: String, rs: ResultSet): FieldMetaData = {
	val column = rs.getString("COLUMN_NAME")
	val clazz = clazzMapping(rs.getString("TYPE_NAME"))
	val sqlType = rs.getString("TYPE_NAME")
	val isAutoIncrement = rs.getString("IS_AUTOINCREMENT") match {
	  case "YES" => true
	  case (_) => false
	}
	new FieldMetaData(tableName, column, clazz, sqlType, isAutoIncrement)
  }
  
}