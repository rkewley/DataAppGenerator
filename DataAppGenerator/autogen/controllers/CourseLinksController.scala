
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

object CourseLinksController extends Controller {

  val formCourseLinks = Form[MdlCourseLinks](
    mapping (
	"fidCourseLinks" -> of[Long],
	"fCourse" -> of[Long],
	"fLink" -> text,
	"fDisplayDescription" -> text,
	"fIsFileLink" -> of[Long]
    )(MdlCourseLinks.apply)(MdlCourseLinks.unapply)
  )
      

  def listCourseLinks = Action {
    Ok(viewlist.html.listCourseLinks(SqlCourseLinks.all))
  }

   def editCourseLinks(id: Long) = Action {
    Ok(viewforms.html.formCourseLinks(formCourseLinks.fill(SqlCourseLinks.select(id)), 0))
  }

   def showCourseLinks(id: Long) = Action {
    Ok(viewshow.html.showCourseLinks(SqlCourseLinks.select(id)))
  }

   def deleteCourseLinks(id: Long) = Action {
    SqlCourseLinks.delete(id)
    Ok(viewlist.html.listCourseLinks(SqlCourseLinks.all))
  }

  def createCourseLinks = Action {
    Ok(viewforms.html.formCourseLinks(formCourseLinks.fill(new MdlCourseLinks()), 1))
  }

  def saveCourseLinks(newEntry: Int) = Action { implicit request =>
  	formCourseLinks.bindFromRequest.fold(
  	  form => {
        val errorMessage = formErrorMessage(form.errors)
        Logger.debug(errorMessage)
        BadRequest(viewforms.html.formError(errorMessage, request.headers("REFERER")))
      },
      vCourseLinks => {
        if (vCourseLinks.validate) {
          newEntry match {
            case 0 => SqlCourseLinks.update(vCourseLinks)
            case _ => SqlCourseLinks.insert(vCourseLinks)
          }
          Redirect(routes.CourseLinksController.listCourseLinks)
        } else {
          val validationErrors = vCourseLinks.validationErrors
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
    errMess("Error Messages:\n", formCourseLinks.errors.toList)
  }
}
