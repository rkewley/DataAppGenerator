
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

object DepartmentsController extends Controller {

  val formDepartments = Form[MdlDepartments](
    mapping (
	"fDepartmentID" -> of[Long],
	"fDepartmentName" -> text,
	"fDepartmentHead" -> text,
	"fDepartmentHopePage" -> text
    )(MdlDepartments.apply)(MdlDepartments.unapply)
  )
      

  def listDepartments = Action {
    Ok(viewlist.html.listDepartments(SqlDepartments.all))
  }

   def editDepartments(id: Long) = Action {
    Ok(viewforms.html.formDepartments(formDepartments.fill(SqlDepartments.select(id)), 0))
  }

   def showDepartments(id: Long) = Action {
    Ok(viewshow.html.showDepartments(SqlDepartments.select(id)))
  }

   def deleteDepartments(id: Long) = Action {
    SqlDepartments.delete(id)
    Ok(viewlist.html.listDepartments(SqlDepartments.all))
  }

  def createDepartments = Action {
    Ok(viewforms.html.formDepartments(formDepartments.fill(new MdlDepartments()), 1))
  }

  def saveDepartments(newEntry: Int) = Action { implicit request =>
  	formDepartments.bindFromRequest.fold(
  	  form => {
        val errorMessage = formErrorMessage(form.errors)
        Logger.debug(errorMessage)
        BadRequest(viewforms.html.formError(errorMessage, request.headers("REFERER")))
      },
      vDepartments => {
        if (vDepartments.validate) {
          newEntry match {
            case 0 => SqlDepartments.update(vDepartments)
            case _ => SqlDepartments.insert(vDepartments)
          }
          Redirect(routes.DepartmentsController.listDepartments)
        } else {
          val validationErrors = vDepartments.validationErrors
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
    errMess("Error Messages:\n", formDepartments.errors.toList)
  }
}
