
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

object CourseReferencesController extends Controller {

  val formCourseReferences = Form[MdlCourseReferences](
    mapping (
	"fidCourseReferences" -> of[Long],
	"fCourse" -> of[Long],
	"fReference" -> of[Long]
    )(MdlCourseReferences.apply)(MdlCourseReferences.unapply)
  )
      

  def listCourseReferences = Action {
    Ok(viewlist.html.listCourseReferences(SqlCourseReferences.all))
  }

   def editCourseReferences(id: Long) = Action {
    Ok(viewforms.html.formCourseReferences(formCourseReferences.fill(SqlCourseReferences.select(id)), 0))
  }

   def showCourseReferences(id: Long) = Action {
    Ok(viewshow.html.showCourseReferences(SqlCourseReferences.select(id)))
  }

   def deleteCourseReferences(id: Long) = Action {
    SqlCourseReferences.delete(id)
    Ok(viewlist.html.listCourseReferences(SqlCourseReferences.all))
  }

  def createCourseReferences = Action {
    Ok(viewforms.html.formCourseReferences(formCourseReferences.fill(new MdlCourseReferences()), 1))
  }

  def saveCourseReferences(newEntry: Int) = Action { implicit request =>
  	formCourseReferences.bindFromRequest.fold(
  	  form => {
        val errorMessage = formErrorMessage(form.errors)
        Logger.debug(errorMessage)
        BadRequest(viewforms.html.formError(errorMessage, request.headers("REFERER")))
      },
      vCourseReferences => {
        if (vCourseReferences.validate) {
          newEntry match {
            case 0 => SqlCourseReferences.update(vCourseReferences)
            case _ => SqlCourseReferences.insert(vCourseReferences)
          }
          Redirect(routes.CourseReferencesController.listCourseReferences)
        } else {
          val validationErrors = vCourseReferences.validationErrors
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
    errMess("Error Messages:\n", formCourseReferences.errors.toList)
  }
}
