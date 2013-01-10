package tools

object SqlGenerator {
  
  def genHeader(tableName: String) = {
"""
package persistence

import models.%s
import anorm._
import anorm.SqlParser._
import play.api.db._
import play.api.Play.current
import play.Logger

object %s {
""" format (DataAppGenerator.modelName(tableName), DataAppGenerator.sqlObjectName(tableName))
  }
  
  def genColumnMapping(field: FieldMetaData): String = {
    "get[" + DataAppGenerator.fieldType(field) + "](\"" + field.column + "\")" 
  }
  
  def genAllColumnMappings(fields: List[FieldMetaData]): String = {
    fields.map(genColumnMapping).mkString(" ~\n\t")
  }
  
  def genCaseList(fields: List[FieldMetaData]): String = {
    fields.map({field => 
      DataAppGenerator.valueName(field.column)
    }).mkString(" ~\n\t\t")
  }

  def genValList(fields: List[FieldMetaData]): String = {
    fields.map({field => 
      DataAppGenerator.valueName(field.column)
    }).mkString(",\n\t\t")
  }

  def genMapper(tableName: String, fields:List[FieldMetaData]): String = {
    """
  val %s = {
    %s map { case
    %s =>
    %s(%s)
    }
  }
""" format (DataAppGenerator.valueName(tableName), genAllColumnMappings(fields), genCaseList(fields),
    DataAppGenerator.modelName(tableName), genValList(fields))
  }
  
  def genAll(tableName: String): String = {
    """
  	def all: List[%s] = DB.withConnection { implicit c =>
  		SQL("select * from `%s`").as(%s *)
	}
""" format (DataAppGenerator.modelName(tableName), tableName, DataAppGenerator.valueName(tableName))

  }

  def genSelectWhere(tableName: String): String = {
    """
  	def selectWhere(where: String): List[%s] = DB.withConnection { implicit c =>
  		SQL("select * from `%s` WHERE " + where).as(%s *)
	}
""" format (DataAppGenerator.modelName(tableName), tableName, DataAppGenerator.valueName(tableName))

  }

  def genKeyFields(fields: List[FieldMetaData]): String = {
    fields.map(keyField => DataAppGenerator.valueName(keyField.column) + ": " + DataAppGenerator.fieldType(keyField)).mkString(", ")
  }
  
  def genWhereFields(fields: List[FieldMetaData]): String = {
    fields.map(keyField => "`" + keyField.column + "` = {" + DataAppGenerator.sqlName(keyField.column) + "}")mkString(" AND ")
  }
  
  def genOnFields(fields: List[FieldMetaData]): String = {
    fields.map(keyField => "'" + DataAppGenerator.sqlName(keyField.column) + " -> " + DataAppGenerator.valueName(keyField.column)).mkString(", ")
  }
  
  def genSelect(tableName: String, primaryKeys: List[FieldMetaData]) : String = {
    val keyFields = genKeyFields(primaryKeys)
    val whereFields = genWhereFields(primaryKeys)
    val onFields = genOnFields(primaryKeys)
    """
	def select(%s): %s = DB.withConnection { implicit c =>
  		SQL("select * from `%s` WHERE %s").on(
  			%s).as(%s *).head
	}
""" format (keyFields, DataAppGenerator.modelName(tableName), tableName, whereFields, onFields, DataAppGenerator.valueName(tableName))
  }
  
  
  def genDelete(tableName: String, primaryKeys: List[FieldMetaData]) : String = {
    val keyFields = genKeyFields(primaryKeys)
    val whereFields = genWhereFields(primaryKeys)
    val onFields = genOnFields(primaryKeys)

    """
	def delete(%s) = DB.withConnection { implicit c =>
  		SQL("DELETE FROM `%s` WHERE %s").on(
      %s
  		).executeUpdate()
    }
""" format (keyFields, tableName, whereFields, onFields)
  }

  def genColumnFields(fields: List[FieldMetaData]): String = {
  	 fields.map(field => "`" + field.column + "`").mkString(", ")
  }
  	
  def genValueFields(fields: List[FieldMetaData]): String = {
    fields.map(field => "{" + DataAppGenerator.sqlName(field.column) + "}").mkString(", ")
  }
  
  def genTableOnFields(tableName: String, fields: List[FieldMetaData]): String = {
    fields.map(keyField => "'" + DataAppGenerator.sqlName(keyField.column) + " -> " + DataAppGenerator.valueName(tableName) +
        "." + DataAppGenerator.valueName(keyField.column)).mkString(", ")
  }
  
  def genInsert(tableName: String, fields: List[FieldMetaData]): String = {
    val insertFields = fields.filter(field => !field.isAutoIncrement)
    val columnFields = genColumnFields(insertFields)
    val valueFields = genValueFields(insertFields)
    val tableOnFields = genTableOnFields(tableName, insertFields)
    val valueName = DataAppGenerator.valueName(tableName)
    val modelName = DataAppGenerator.modelName(tableName)
    """
	def insert(%s: %s) = DB.withConnection { implicit c =>
  		SQL("INSERT INTO `%s` (%s) VALUES (%s)").on(%s).executeInsert()
	}
""" format (valueName, modelName, tableName, columnFields, valueFields, tableOnFields)
  }
  
  def genUpdateFields(fields: List[FieldMetaData]): String = {
    fields.map(field => "`" + field.column + "` = {" + DataAppGenerator.sqlName(field.column) + "}").mkString(", ")
  }
  
  def genUpdate(tableName: String, fields: List[FieldMetaData], primaryKeys: List[FieldMetaData], db: DB): String = {
    val valueName = DataAppGenerator.valueName(tableName)
    val modelName = DataAppGenerator.modelName(tableName)
    val nonkeyFields = fields.filter(field => !db.isKey(field, primaryKeys))
    val updateFields = genUpdateFields(nonkeyFields)
    val tableOnFields = genTableOnFields(tableName, fields)
    val whereFields = genWhereFields(primaryKeys)
    """
	def update(%s: %s) = DB.withConnection { implicit c =>
  		SQL("UPDATE `%s` SET %s WHERE %s").on(%s).executeUpdate()

  }
""" format(valueName, modelName, tableName, updateFields, whereFields, tableOnFields)
  }
  
  def writeSql(tableName:String, db: DB) {
    val fields = db.getFieldList(tableName)
    val primaryKeys = db.getPrimaryKeys(tableName)
    val sqlData = genHeader(tableName) + genMapper(tableName, fields) + genAll(tableName) + 
    	genSelect(tableName, primaryKeys) + genSelectWhere(tableName) + genDelete(tableName, primaryKeys) + genInsert(tableName, fields) + 
    	genUpdate(tableName, fields, primaryKeys, db) + "\n}"
    println(sqlData)
    DataAppGenerator.writeStringToFile("autogen/persistence/" + DataAppGenerator.sqlObjectName(tableName) + ".scala", sqlData)

  }


}