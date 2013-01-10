
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

object CoursesController extends Controller {

  val formCourses = Form[MdlCourses](
    mapping (
	"fidCourse" -> of[Long],
	"fCourseIDNumber" -> text,
	"fAcademicYear" -> of[Long],
	"fAcademicTerm" -> of[Long],
	"fCourseName" -> text,
	"fCourseDirectorEmail" -> text,
	"fProgramDirectorEmail" -> text,
	"fCourseDescriptionRedbook" -> text,
	"fCreditHours" -> of[Double],
	"fPrerequisites" -> text,
	"fCorequisites" -> text,
	"fDisqualifiers" -> text,
	"fCourseStrategy" -> text,
	"fCriteriaForPassing" -> text,
	"fAdminInstructions" -> text,
	"fDepartmentID" -> of[Long],
	"fCourseWebsite" -> of[Boolean],
	"fCourseDescriptionWebsite" -> text
    )(MdlCourses.apply)(MdlCourses.unapply)
  )
      

  def listCourses = Action {
    Ok(viewlist.html.listCourses(SqlCourses.all))
  }

   def editCourses(id: Long) = Action {
    Ok(viewforms.html.formCourses(formCourses.fill(SqlCourses.select(id)), 0))
  }

   def showCourses(id: Long) = Action {
    Ok(viewshow.html.showCourses(SqlCourses.select(id)))
  }

   def deleteCourses(id: Long) = Action {
    SqlCourses.delete(id)
    Ok(viewlist.html.listCourses(SqlCourses.all))
  }

  def createCourses = Action {
    Ok(viewforms.html.formCourses(formCourses.fill(new MdlCourses()), 1))
  }

  def saveCourses(newEntry: Int) = Action { implicit request =>
  	formCourses.bindFromRequest.fold(
  	  form => {
        val errorMessage = formErrorMessage(form.errors)
        Logger.debug(errorMessage)
        BadRequest(viewforms.html.formError(errorMessage, request.headers("REFERER")))
      },
      vCourses => {
        if (vCourses.validate) {
          newEntry match {
            case 0 => SqlCourses.update(vCourses)
            case _ => SqlCourses.insert(vCourses)
          }
          Redirect(routes.CoursesController.listCourses)
        } else {
          val validationErrors = vCourses.validationErrors
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
    errMess("Error Messages:\n", formCourses.errors.toList)
  }
}
