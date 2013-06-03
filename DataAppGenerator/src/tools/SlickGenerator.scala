package tools

object SlickGenerator {
  
  def genHeader(tableName: String, primaryKey: FieldMetaData) = {
    val modelName = DataAppGenerator.modelName(tableName)
    val noWhitespace = DataAppGenerator.noWhitespace(tableName)
"""
package slick

import models.%s
import models.Mdl

trait %sComponent  {
	this: Profile =>
	  
	import profile.simple._

	object %s extends Table[%s]("Programs") with Crud[%s, %s]  {
""" format (modelName, 
    noWhitespace,
    noWhitespace, modelName, modelName, DataAppGenerator.fieldType(primaryKey))
  }
  
  def genColumnMapping(field: FieldMetaData): String = {
    val columnName = DataAppGenerator.noWhitespace(field.column)
    val valueName = DataAppGenerator.valueName(field.column)
    val columnType = DataAppGenerator.fieldType(field)
"""def %s = column[%s]("%s"""" format(valueName, columnType, columnName)
  }
  
  def genAllColumnMappings(fields: List[FieldMetaData], primaryKey: FieldMetaData): String = {
    "\n      " +
    fields.map(field => 
      (primaryKey.column == field.column) match {
        case true =>
          genColumnMapping(field) + ", O.PrimaryKey)"
        case false =>
          genColumnMapping(field) + ")"
      }).mkString("\n      ")
  }
  
  def genColumnNames(fields: List[FieldMetaData], prepend: String, separator: String) = {
    fields.map(field=> prepend + DataAppGenerator.valueName(field.column)).mkString(separator)
  }
  
  def genTupleFields(fields: List[FieldMetaData]): String = {
    def genTupleField(tupleFields: String, i: Int): String = {
      if(i >= fields.size) {
        tupleFields
      }
      else {
        val newTupleFields = tupleFields + ", t._" + i
        genTupleField(newTupleFields, i+1)
      }
    }
    genTupleField("(None ", 1)
  }
  
  def genStar(tableName: String, fields: List[FieldMetaData], primaryKey: FieldMetaData): String = {
    val modelName = DataAppGenerator.modelName(tableName)
    "\n" +
    "      def * = " + DataAppGenerator.valueName(primaryKey.column) + ".? ~ " + 
    genColumnNames(fields.filter(field => field.column != primaryKey.column), "", " ~ ") + 
    "<> (" + modelName + ".apply _, " + modelName + ".unapply _)\n"
  }
  
  def genForInsert(tableName: String, fields: List[FieldMetaData], primaryKey: FieldMetaData): String = {
    val modelName = DataAppGenerator.modelName(tableName)
    val valueName = DataAppGenerator.valueName(tableName)
    "      def forInsert = " + genColumnNames(fields.filter(field => field.column != primaryKey.column), "", " ~ ") + " <> \n" +
    "      ({t => " + modelName + genTupleFields(fields) + ")},\n" + 
    "      {(" + valueName + ": " + modelName + ") => Some(" + genColumnNames(fields.filter(field => field.column != primaryKey.column), valueName + ".", ", ") + ")})\n"
  }
  
  
  def genStandardMethods(tableName: String, fields: List[FieldMetaData], primaryKey: FieldMetaData) = {
	val primaryKeyType = DataAppGenerator.fieldType(primaryKey)
	val noWhitespace = DataAppGenerator.noWhitespace(tableName)
"""
	  def allQuery = {
	    AppDB.database.withSession { implicit session: Session =>
	      Query(%s)
	    }
	  }
	  
	  def all = {
	    AppDB.database.withSession { implicit session: Session =>
	      val q = Query(%s)
	      q.elements.toList
	    }
	  }
	  
	  def selectQuery(pk: %s)(implicit session: Session) = {
	      val q = Query(%s)
	      q.filter(p => p.%s === pk)
	  }

      def select(pk: %s) = {
	    AppDB.database.withSession { implicit session: Session =>
	      selectQuery(pk).elements.toList.headOption
	    }
	  }
	  
	  def delete(pk: %s) {
	    AppDB.database.withSession { implicit session: Session =>
	      selectQuery(pk).delete
	    }
	  }
""" format(noWhitespace,
    noWhitespace,
    primaryKeyType,
    noWhitespace,
    DataAppGenerator.valueName(primaryKey.column),
    primaryKeyType,
    primaryKeyType)
  }
  
  def genInsert(tableName: String, fields: List[FieldMetaData], primaryKey: FieldMetaData): String = {  
    val valueName = DataAppGenerator.valueName(tableName)
    val modelName = DataAppGenerator.modelName(tableName)
    val noWhitespace = DataAppGenerator.noWhitespace(tableName)
    val insertMethod =
"""
	  def insert(%s: %s) {
	    AppDB.database.withSession { implicit session: Session =>
	      %s.forInsert insert %s(None, """ format(valueName, modelName,noWhitespace, modelName)
	insertMethod + genColumnNames(fields.filter(field => field.column != primaryKey.column), valueName + ".", ", ") + ")\n" + """
	    }
	  }
    
"""    
  }
  
  def genUpdate(tableName: String, fields: List[FieldMetaData], primaryKey: FieldMetaData): String = {
    val valueName = DataAppGenerator.valueName(tableName)
    val pkValueName = DataAppGenerator.valueName(primaryKey.column)
    val modelName = DataAppGenerator.modelName(tableName)
    val noWhitespace = DataAppGenerator.noWhitespace(tableName)
"""
	  def update(%s: %s) {
	    AppDB.database.withSession { implicit session: Session =>
	      val q = selectQuery(%s.%s.get)
	      val q2 = q.map(%s => %s)
	      q2.update(%s)
	    }
	  }
"""   format(valueName, modelName,
		valueName, pkValueName, 
		valueName, genColumnNames(fields.filter(field => field.column != primaryKey.column), valueName + ".", " ~ "),
		genColumnNames(fields.filter(field => field.column != primaryKey.column), valueName + ".", ", "))
  }
  
  def genEnd = """
	}
}
"""
    
  
  def writeSlick(tableName:String, db: DB) {
    val fields = db.getFieldList(tableName)
    val primaryKey = db.getPrimaryKeys(tableName).headOption.getOrElse(
        throw new Exception("Can't generate Slick object for table " + tableName + " with no primary key"))
    val slickData = genHeader(tableName, primaryKey) + genAllColumnMappings(fields, primaryKey) +
      genStar(tableName, fields, primaryKey) + genForInsert(tableName, fields, primaryKey) + genStandardMethods(tableName, fields, primaryKey) +
      genInsert(tableName, fields, primaryKey) + genUpdate(tableName, fields, primaryKey) + genEnd
    println(slickData)
    DataAppGenerator.writeStringToFile("autogen/slick/" + DataAppGenerator.slickObjectName(tableName) + ".scala", slickData)

  }


}