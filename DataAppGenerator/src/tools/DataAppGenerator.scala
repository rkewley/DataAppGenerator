package tools

import scala.sys.process._
import java.io._


object DataAppGenerator {
  
  def fieldType(field:FieldMetaData):String = {
    val t = field.clazz match {
      case "java.lang.String" => "String"
      case "java.lang.Boolean" => "Boolean"
      case "java.lang.Integer" => "Long"
      case "java.lang.Long" => "Long"
      case "java.math.BigDecimal" => "BigDecimal"
      case "java.lang.Double" => "Double"
     case other => other
    }
    //if (field.nullable) "Option[%s]" format (t)
    //else t
    t
  }
  
  /**
   * Drop the schema name from a string (tablename or fieldname)
   */
  def dropSchemaName(str:String):String = 
    str.dropWhile(c => c != '.').drop(1)
   
  def modelName(tableName: String) = ("Mdl" + tableName).replaceAll(" ", "")
  def valueName(fieldName: String) = ("v" + fieldName).replaceAll(" ", "")
  def sqlName(fieldName: String) = ("sql" + fieldName).replaceAll(" ", "")
  def formValue(fieldName: String) = ("f" + fieldName).replaceAll(" ", "")
  def sqlObjectName(tableName: String)   = ("Sql" + tableName).replaceAll(" ", "")
  def formName(tableName: String) = ("form" + tableName).replaceAll(" ", "")
  def showName(tableName: String) = ("show" + tableName).replaceAll(" ", "")
  def listName(tableName: String) = ("list" + tableName).replaceAll(" ", "")
  def createName(tableName: String) = ("create" + tableName).replaceAll(" ", "")
  def deleteName(tableName: String) = ("delete" + tableName).replaceAll(" ", "")
  def saveName(tableName: String) = ("save" + tableName).replaceAll(" ", "")
  def editName(tableName: String) = ("edit" + tableName).replaceAll(" ", "")
  def noWhitespace(tableName: String) = tableName.replaceAll(" ", "")
  def controllerName(tableName: String) = (tableName + "Controller").replaceAll(" ", "")

  
  def writeStringToFile(fileName: String, data: String) {
    println("File name is: " + fileName)
    val writer = new PrintWriter(new File(fileName))
    writer.write(data)
    writer.close()
  }
  
  def setInSiteContext(contextName: String, sideBar: String): String = {"""
    @main("Systems Net", "%s") {
	<div>
	  <div class="column span-4">
		 %s
	  </div>
""" format (contextName, sideBar)
  }
  
  
  
  def main(args: Array[String]) {
    
    val database = new tools.DB
    args.foreach(arg => {
      println("Arg: " + arg)
      val split = arg.split("~")
      println("Splits into " + split.size + " pieces")
      val tableName_fixedForeignKey:(String, Option[ForeignKeyMetaData]) = split.size match {
        case 2 => {
          val tableName = split(0)
          val fixedForeignKeyName = split(1)
          println("Tablename is " + tableName + " and fixed foreign key name is " + fixedForeignKeyName)
          (split(0), database.getForeignKeys(tableName).find(foreignKey => foreignKey.pk.table == split(1)))
        }
        case (_) => {
          (split(0), None)
        }
      }
        val tableName = tableName_fixedForeignKey._1
        val fixedForeignKey = tableName_fixedForeignKey._2
        val fkName = fixedForeignKey match {
          case Some(foreignKey) => foreignKey.fk.column
          case None => "not defined"
        }
        println("Fixed foreign key column is " + fkName)
	    ModelGenerator.writeModel(tableName, database)
	    ControllerGenerator.writeController(tableName, database, fixedForeignKey)
	    SqlGenerator.writeSql(tableName, database)
	    FormGenerator.writeForm(tableName, database, fixedForeignKey)
	    RouteGenerator.writeRoutes(tableName, database, fixedForeignKey)
	    ListGenerator.writeListForm(tableName, database, fixedForeignKey)
	    ShowGenerator.writeShowView(tableName, database, fixedForeignKey)          
	})
  }
}