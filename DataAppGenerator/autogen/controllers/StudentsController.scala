
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

object StudentsController extends Base {

  val formStudents = Form[MdlStudents](
    mapping (
	"fidStudent" -> of[Long],
	"fEmail" -> text,
	"fLastName" -> text,
	"fFirstName" -> text,
	"fPreferredFirstName" -> text,
	"fCadetCompany" -> text,
	"fMajor" -> text,
	"fBarracksRoom" -> text,
	"fCellPhone" -> text,
	"fHomeTown" -> text,
	"fCadetSports" -> text,
	"fCadetActivities" -> text,
	"fPlannedBranch" -> text,
	"fPlannedPost" -> text,
	"fExpectationsFromCourse" -> text,
	"fTopicsToDiscuss" -> text
    )(MdlStudents.apply)(MdlStudents.unapply)
  )
      

  def listStudents = Action {
    Ok(viewlist.html.listStudents(SqlStudents.all))
  }

   def editStudents(id: Long) = compositeAction(NormalUser) { user => implicit template => implicit request =>
    Ok(viewforms.html.formStudents(formStudents.fill(SqlStudents.select(id)), 0))
  }

   def showStudents(id: Long) = Action {
    Ok(viewshow.html.showStudents(SqlStudents.select(id)))
  }

   def deleteStudents(id: Long) = compositeAction(NormalUser) { user => implicit template => implicit request =>
    SqlStudents.delete(id)
    Ok(viewlist.html.listStudents(SqlStudents.all))
  }

  def createStudents = compositeAction(NormalUser) { user => implicit template => implicit request =>
    Ok(viewforms.html.formStudents(formStudents.fill(new MdlStudents()), 1))
  }

  def saveStudents(newEntry: Int) = Action { implicit request =>
  	formStudents.bindFromRequest.fold(
  	  form => {
        val errorMessage = formErrorMessage(form.errors)
        Logger.debug(errorMessage)
        BadRequest(viewforms.html.formError(errorMessage, request.headers("REFERER")))
      },
      vStudents => {
        if (vStudents.validate) {
          newEntry match {
            case 0 => SqlStudents.update(vStudents)
            case _ => SqlStudents.insert(vStudents)
          }
          Redirect(routes.StudentsController.listStudents)
        } else {
          val validationErrors = vStudents.validationErrors
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
    errMess("Error Messages:\n", formStudents.errors.toList)
  }
}
