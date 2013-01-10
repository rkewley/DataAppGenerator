
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

object ProgramsController extends Controller {

  val formPrograms = Form[MdlPrograms](
    mapping (
	"fProgram" -> text,
	"fName" -> text,
	"fSlogan" -> text,
	"fInformation" -> text,
	"fProgramDirector" -> text,
	"fDepartment" -> of[Long]
    )(MdlPrograms.apply)(MdlPrograms.unapply)
  )
      

  def listPrograms = Action {
    Ok(viewlist.html.listPrograms(SqlPrograms.all))
  }

   def editPrograms(id: String) = Action {
    Ok(viewforms.html.formPrograms(formPrograms.fill(SqlPrograms.select(id)), 0))
  }

   def showPrograms(id: String) = Action {
    Ok(viewshow.html.showPrograms(SqlPrograms.select(id)))
  }

   def deletePrograms(id: String) = Action {
    SqlPrograms.delete(id)
    Ok(viewlist.html.listPrograms(SqlPrograms.all))
  }

  def createPrograms = Action {
    Ok(viewforms.html.formPrograms(formPrograms.fill(new MdlPrograms()), 1))
  }

  def savePrograms(newEntry: Int) = Action { implicit request =>
  	formPrograms.bindFromRequest.fold(
  	  form => {
        val errorMessage = formErrorMessage(form.errors)
        Logger.debug(errorMessage)
        BadRequest(viewforms.html.formError(errorMessage, request.headers("REFERER")))
      },
      vPrograms => {
        if (vPrograms.validate) {
          newEntry match {
            case 0 => SqlPrograms.update(vPrograms)
            case _ => SqlPrograms.insert(vPrograms)
          }
          Redirect(routes.ProgramsController.listPrograms)
        } else {
          val validationErrors = vPrograms.validationErrors
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
    errMess("Error Messages:\n", formPrograms.errors.toList)
  }
}
