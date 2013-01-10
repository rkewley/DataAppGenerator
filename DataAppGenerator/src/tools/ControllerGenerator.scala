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
  
    def genForm(tableName: String, fields: List[FieldMetaData]): String = {
    val header = """  val %s = Form[%s](
    mapping (
""" format (DataAppGenerator.formName(tableName), DataAppGenerator.modelName(tableName))
    
    def genFormField(field:FieldMetaData):String = {
      "\t\"" + DataAppGenerator.formValue(field.column) + "\" -> " + mappingType(field)
    }
    
    val formFields = fields.map(genFormField).mkString(",\n")
    
    val formApply = """
    )(%s.apply)(%s.unapply)
  )
      
""" format(DataAppGenerator.modelName(tableName), DataAppGenerator.modelName(tableName))

    header + formFields + formApply
  }

  def genControllerHeader(tableName: String): String = {"""
package controllers

import play.api._
import play.api.mvc._
import play.api.data._
import models._
import persistence._
import play.Logger
import play.api.data._
import play.api.data.Forms._
import play.api.data.format.Formats._
import FormFieldImplicits._

object %sController extends Base {

""" format (tableName)
  }
  
  def genListing(tableName: String): String = {
"""
  def %s = Action {
    Ok(viewlist.html.%s(%s.all))
  }
""" format (DataAppGenerator.listName(tableName), DataAppGenerator.listName(tableName), 
		DataAppGenerator.sqlObjectName(tableName))
  }
  
  def genListingFk(tableName: String, foreignKey: ForeignKeyMetaData): String = {
"""
  def %s(id%s: %s) = Action {
     Ok(viewlist.html.%s(%s.selectWhere("`%s` = " + id%s), id%s))
  }
""" format (DataAppGenerator.listName(tableName), DataAppGenerator.noWhitespace(foreignKey.pk.table), DataAppGenerator.fieldType(foreignKey.fk),
    DataAppGenerator.listName(tableName), DataAppGenerator.sqlObjectName(tableName), foreignKey.fk.column, DataAppGenerator.noWhitespace(foreignKey.pk.table), DataAppGenerator.noWhitespace(foreignKey.pk.table))
  }
  
  def genEdit(tableName: String, primaryKey: FieldMetaData): String = {
"""
   def %s(id: %s) = compositeAction(NormalUser) { user => implicit template => implicit request =>
    Ok(viewforms.html.%s(%s.fill(%s.select(id)), 0))
  }
""" format(DataAppGenerator.editName(tableName), DataAppGenerator.fieldType(primaryKey), 
		DataAppGenerator.formName(tableName), DataAppGenerator.formName(tableName), DataAppGenerator.sqlObjectName(tableName))
  }
  
  def genDelete(tableName: String, primaryKey: FieldMetaData): String = {
"""
   def delete%s(id: %s) = compositeAction(NormalUser) { user => implicit template => implicit request =>
    %s.delete(id)
    Ok(viewlist.html.%s(%s.all))
  }
""" format(DataAppGenerator.noWhitespace(tableName), DataAppGenerator.fieldType(primaryKey), DataAppGenerator.sqlObjectName(tableName), 
		DataAppGenerator.listName(tableName), DataAppGenerator.sqlObjectName(tableName))
  }
  
  def genDeleteFk(tableName: String, primaryKey: FieldMetaData, foreignKey: ForeignKeyMetaData): String = {
"""
   def delete%s(id: %s) = compositeAction(NormalUser) { user => implicit template => implicit request =>
    val %s = %s.select(id)
    %s.delete(id)
    Redirect(routes.%s.%s(%s.%s))
  }
""" format(DataAppGenerator.noWhitespace(tableName), DataAppGenerator.fieldType(primaryKey), 
    DataAppGenerator.valueName(tableName), DataAppGenerator.sqlObjectName(tableName),
    DataAppGenerator.sqlObjectName(tableName), 
	DataAppGenerator.controllerName(tableName), DataAppGenerator.listName(tableName), DataAppGenerator.valueName(tableName), DataAppGenerator.valueName(foreignKey.fk.column))
  }
  def genShow(tableName: String, primaryKey: FieldMetaData): String = {
"""
   def %s(id: %s) = Action {
    Ok(viewshow.html.%s(%s.select(id)))
  }
""" format(DataAppGenerator.showName(tableName), DataAppGenerator.fieldType(primaryKey),
		DataAppGenerator.showName(tableName), DataAppGenerator.sqlObjectName(tableName))
  }
  def genCreate(tableName: String): String = {
"""
  def %s = compositeAction(NormalUser) { user => implicit template => implicit request =>
    Ok(viewforms.html.%s(%s.fill(new %s()), 1))
  }
""" format (DataAppGenerator.createName(tableName), DataAppGenerator.formName(tableName), DataAppGenerator.formName(tableName), DataAppGenerator.modelName(tableName))
  }
  
  def genCreateFk(tableName: String, foreignKey: ForeignKeyMetaData): String = {
"""
  def %s(id%s: %s) = compositeAction(NormalUser) { user => implicit template => implicit request =>
    val %s = new %s(You need to put the default values here setting the fixed foreign key)
    Ok(viewforms.html.%s(%s.fill(%s), 1))
  }
""" format (DataAppGenerator.createName(tableName), DataAppGenerator.noWhitespace(foreignKey.pk.table), DataAppGenerator.fieldType(foreignKey.fk),
    DataAppGenerator.valueName(tableName), DataAppGenerator.modelName(tableName),
    DataAppGenerator.formName(tableName), DataAppGenerator.formName(tableName), DataAppGenerator.valueName(tableName))
  }
  

  def genSave(tableName: String): String = {
    val tn = tableName
    val saveName = DataAppGenerator.saveName(tableName)
    val formName = DataAppGenerator.formName(tableName)
    val valueName = DataAppGenerator.valueName(tableName)
    val sqlTableName = DataAppGenerator.sqlObjectName(tableName)
    val controllerName = DataAppGenerator.controllerName(tableName)
    val listName = DataAppGenerator.listName(tableName)
    return {
"""
  def %s(newEntry: Int) = Action { implicit request =>
  	%s.bindFromRequest.fold(
  	  form => {
        val errorMessage = formErrorMessage(form.errors)
        Logger.debug(errorMessage)
        BadRequest(viewforms.html.formError(errorMessage, request.headers("REFERER")))
      },
      %s => {
        if (%s.validate) {
          newEntry match {
            case 0 => %s.update(%s)
            case _ => %s.insert(%s)
          }
          Redirect(routes.%s.%s)
        } else {
          val validationErrors = %s.validationErrors
          Logger.debug(validationErrors)
          BadRequest(viewforms.html.formError(validationErrors, request.headers("REFERER")))
        }
      })
  }
    
  def formErrorMessage(errors: Seq[FormError]) = {
    def errMess(message: String, errorList: List[FormError]): String = {
      if (errorList.isEmpty) message else {
        errMess(message + errorList.head.message + "\n", errorList.tail)
      }
    }
    errMess("Error Messages:\n", %s.errors.toList)
  }
}
""" format (saveName, formName, valueName, valueName, sqlTableName, valueName, sqlTableName, valueName, controllerName, 
		listName, valueName, formName)}  
  }
  
  def genSaveFk(tableName: String, foreignKey: ForeignKeyMetaData): String = {
    val tn = tableName
    val saveName = DataAppGenerator.saveName(tableName)
    val formName = DataAppGenerator.formName(tableName)
    val valueName = DataAppGenerator.valueName(tableName)
    val sqlTableName = DataAppGenerator.sqlObjectName(tableName)
    val controllerName = DataAppGenerator.controllerName(tableName)
    val listName = DataAppGenerator.listName(tableName)
    return {
"""
  def %s(newEntry: Int) = Action { implicit request =>
  	%s.bindFromRequest.fold(
  	  form => {
        val errorMessage = formErrorMessage(form.errors)
        Logger.debug(errorMessage)
        BadRequest(viewforms.html.formError(errorMessage, request.headers("REFERER")))
      },
      %s => {
        if (%s.validate) {
          newEntry match {
            case 0 => %s.update(%s)
            case _ => %s.insert(%s)
          }
          Redirect(routes.%s.%s(%s.%s))
        } else {
          val validationErrors = %s.validationErrors
          Logger.debug(validationErrors)
          BadRequest(viewforms.html.formError(validationErrors, request.headers("REFERER")))
        }
      })
  }
    
  def formErrorMessage(errors: Seq[FormError]) = {
    def errMess(message: String, errorList: List[FormError]): String = {
      if (errorList.isEmpty) message else {
        errMess(message + errorList.head.message + "\n", errorList.tail)
      }
    }
    errMess("Error Messages:\n", %s.errors.toList)
  }
}
""" format (saveName, 
    formName, 
    valueName, 
    valueName, 
    sqlTableName, valueName, 
    sqlTableName, valueName, 
    controllerName, listName, valueName, DataAppGenerator.valueName(foreignKey.fk.column),
    valueName, formName)}  
  }
  

  
  
  def writeController(tableName:String, db: DB, fixedForeignKey: Option[ForeignKeyMetaData]) {
    val fields = db.getFieldList(tableName)
    val primaryKey = db.getPrimaryKeys(tableName).head
    
    val controllerData = fixedForeignKey match {
      case Some(fk) => {
        val controllerHeader = genControllerHeader(tableName)
        val form = genForm(tableName, fields)
        val methods = genListingFk(tableName, fk) + genEdit(tableName, primaryKey) + genShow(tableName, primaryKey) +
    	genDeleteFk(tableName, primaryKey, fk) + genCreateFk(tableName, fk) + genSaveFk(tableName, fk)
        controllerHeader + form + methods    
      }
      case (_) => {
        val controllerHeader = genControllerHeader(tableName)
        val form = genForm(tableName, fields)
        val methods = genListing(tableName) + genEdit(tableName, primaryKey) + genShow(tableName, primaryKey) +
    	 genDelete(tableName, primaryKey) + genCreate(tableName) + genSave(tableName)
        controllerHeader + form + methods
        
      }
    }
    DataAppGenerator.writeStringToFile("autogen/controllers/" + tableName + "Controller.scala", controllerData)
  }
}
