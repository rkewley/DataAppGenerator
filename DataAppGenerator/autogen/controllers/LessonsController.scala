
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

object LessonsController extends Base {

  val formLessons = Form[MdlLessons](
    mapping (
	"fLessonIndex" -> of[Long],
	"fLessonNumber" -> of[Long],
	"fLessonName" -> text,
	"fAssignment" -> text,
	"fLocation" -> text,
	"fidCourse" -> of[Long],
	"fDuration" -> of[Long],
	"fLab" -> of[Boolean],
	"fLessonSummary" -> text
    )(MdlLessons.apply)(MdlLessons.unapply)
  )
      

  def listLessons = Action {
    Ok(viewlist.html.listLessons(SqlLessons.all))
  }

   def editLessons(id: Long) = compositeAction(NormalUser) { user => implicit template => implicit request =>
    Ok(viewforms.html.formLessons(formLessons.fill(SqlLessons.select(id)), 0))
  }

   def showLessons(id: Long) = Action {
    Ok(viewshow.html.showLessons(SqlLessons.select(id)))
  }

   def deleteLessons(id: Long) = compositeAction(NormalUser) { user => implicit template => implicit request =>
    SqlLessons.delete(id)
    Ok(viewlist.html.listLessons(SqlLessons.all))
  }

  def createLessons = compositeAction(NormalUser) { user => implicit template => implicit request =>
    Ok(viewforms.html.formLessons(formLessons.fill(new MdlLessons()), 1))
  }

  def saveLessons(newEntry: Int) = Action { implicit request =>
  	formLessons.bindFromRequest.fold(
  	  form => {
        val errorMessage = formErrorMessage(form.errors)
        Logger.debug(errorMessage)
        BadRequest(viewforms.html.formError(errorMessage, request.headers("REFERER")))
      },
      vLessons => {
        if (vLessons.validate) {
          newEntry match {
            case 0 => SqlLessons.update(vLessons)
            case _ => SqlLessons.insert(vLessons)
          }
          Redirect(routes.LessonsController.listLessons)
        } else {
          val validationErrors = vLessons.validationErrors
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
    errMess("Error Messages:\n", formLessons.errors.toList)
  }
}
