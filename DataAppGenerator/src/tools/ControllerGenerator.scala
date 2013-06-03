package tools


object ControllerGenerator {
  
  def mappingType(field:FieldMetaData): String = {
    field.clazz match {
      case "java.lang.String" => "text"
      case "java.lang.Boolean" => "of[Boolean]"
      case "java.lang.Integer" => "of[Long]"
      case "java.math.BigDecimal" => "of[Double]"
      case "java.lang.Double" => "of[Double]"
     case other => other
    }
  }
  
  def genForm(tableName: String, fields: List[FieldMetaData], primaryKey: FieldMetaData): String = {
    val header = """  val form = Form[%s](
    mapping (
""" format (DataAppGenerator.modelName(tableName))
    
    def genFormField(field:FieldMetaData):String = {
      "\t\"" + DataAppGenerator.formValue(field.column) + "\" -> " + mappingType(field)
    }
    
    def genPkFormField(field:FieldMetaData):String = {
      "\t\"" + DataAppGenerator.formValue(field.column) + "\" -> optional(" + mappingType(field) + ")"
    }
    
    val formFields = fields.map(field => 
      (field.column == primaryKey.column) match {
        case true =>
          genPkFormField(field)
        case false =>
          genFormField(field)
      }).mkString(",\n")
    
    val formApply = """
    )(%s.apply)(%s.unapply)
  )
      
""" format(DataAppGenerator.modelName(tableName), DataAppGenerator.modelName(tableName))

    header + formFields + formApply
  }

  def genControllerHeader(tableName: String, primaryKey: FieldMetaData, fixedForeignKeyType: String): String = {"""
package controllers

import play.api._
import play.api.templates._
import play.api.data._
import play.api.data.Forms._
import play.api.data.format.Formats._
import models._
import views._

object %sController extends ControllerTrait[%s, %s, %s] with Base {

""" format (tableName, DataAppGenerator.fieldType(primaryKey), DataAppGenerator.modelName(tableName), fixedForeignKeyType)
  }
  
  def genDefinitions(tableName: String, primaryKey: FieldMetaData) = { 
    
    val modelName = DataAppGenerator.modelName(tableName)
    val valueName = DataAppGenerator.valueName(tableName)
    val noWhiteSpace = DataAppGenerator.noWhitespace(tableName)
"""
	override def listFunction(list%s: List[%s]): Html = 
	  views.html.viewlist.list%s(list%s)
 
	override def showFunction(%s: %s): Html = 
	  views.html.viewshow.show%s(%s)
	
	override def editFunction(mdl%sForm: Form[%s]): Html = 
	  views.html.viewforms.form%s(mdl%sForm, 0)
	
	override def createFunction(mdl%sForm: Form[%s]): Html = 
	  views.html.viewforms.form%s(mdl%sForm, 1)
	  
	def crud = slick.AppDB.dal.%s

""" format (modelName, modelName,
			noWhiteSpace, modelName,
			
			valueName, modelName,
			noWhiteSpace, valueName,
			
			noWhiteSpace, modelName,
			noWhiteSpace, noWhiteSpace,
			
			noWhiteSpace, modelName,
			noWhiteSpace, noWhiteSpace,
			
			noWhiteSpace)
  }
  
  def genNewItem(tableName: String) = { """
	def newItem(fkId: Long) = new %s""" format(DataAppGenerator.noWhitespace(tableName))
  }
    
  def genNewItemFk(tableName: String, fixedForeignKey: ForeignKeyMetaData) = {
    val valueName = DataAppGenerator.valueName(tableName)
    val modelName = DataAppGenerator.modelName(tableName)
    val noWhitespace = DataAppGenerator.noWhitespace(tableName)
    val fkType = DataAppGenerator.fieldType(fixedForeignKey.fk)
"""
    def newItem(fkId: %s): %s = Write code here to properly generate a new item with proper fixed foreign key
    override def getAll(fkId: %s): List[%s] = Write code here to query database for items with same fixed foreign key
    override def getAll(%s: %s): List[%s] = Write code here to query database for items with same fixed foreign key
""" format(fkType, modelName,
		   fkType, modelName,
		   valueName, modelName, modelName)
  }

  def writeController(tableName: String, db: DB, fixedForeignKey: Option[ForeignKeyMetaData]) {
    val fields = db.getFieldList(tableName)
    val primaryKey = db.getPrimaryKeys(tableName).head
    val form = genForm(tableName, fields, primaryKey)
    val definitions = genDefinitions(tableName, primaryKey)
    val controllerData = db.getPrimaryKeys(tableName).isEmpty match {
      case false =>

        fixedForeignKey match {
          case Some(fk) => {
            println("Contoller has fixed foreign key")
            genControllerHeader(tableName, primaryKey, DataAppGenerator.fieldType(fixedForeignKey.get.fk)) + 
              form + definitions + genNewItemFk(tableName, fixedForeignKey.get)
          }
          case None => {
            println("Contoller does not have fixed foreign key")
            genControllerHeader(tableName, primaryKey, "Long") + form + definitions + genNewItem(tableName) +
              form + definitions + genNewItem(tableName)

          }
        }
      case true =>
        throw new Exception("Cannot generate a controller for a table without a primary key field")
    }

    DataAppGenerator.writeStringToFile("autogen/controllers/" + tableName + "Controller.scala", controllerData)
  }
}
