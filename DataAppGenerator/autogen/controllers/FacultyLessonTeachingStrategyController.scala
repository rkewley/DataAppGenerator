
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

object FacultyLessonTeachingStrategyController extends Base {

  val formFacultyLessonTeachingStrategy = Form[MdlFacultyLessonTeachingStrategy](
    mapping (
	"fFaculty_Email" -> text,
	"fLessons_LessonIndex" -> of[Long],
	"fTeachingStrategy" -> text
    )(MdlFacultyLessonTeachingStrategy.apply)(MdlFacultyLessonTeachingStrategy.unapply)
  )
      

  def listFacultyLessonTeachingStrategy(idLessons: Long) = Action {
     Ok(viewlist.html.listFacultyLessonTeachingStrategy(SqlFacultyLessonTeachingStrategy.selectWhere("`Lessons_LessonIndex` = " + idLessons), idLessons))
  }

   def editFacultyLessonTeachingStrategy(id: String) = compositeAction(NormalUser) { user => implicit template => implicit request =>
    Ok(viewforms.html.formFacultyLessonTeachingStrategy(formFacultyLessonTeachingStrategy.fill(SqlFacultyLessonTeachingStrategy.select(id)), 0))
  }

   def showFacultyLessonTeachingStrategy(id: String) = Action {
    Ok(viewshow.html.showFacultyLessonTeachingStrategy(SqlFacultyLessonTeachingStrategy.select(id)))
  }

   def deleteFacultyLessonTeachingStrategy(id: String) = compositeAction(NormalUser) { user => implicit template => implicit request =>
    val vFacultyLessonTeachingStrategy = SqlFacultyLessonTeachingStrategy.select(id)
    SqlFacultyLessonTeachingStrategy.delete(id)
    Redirect(routes.FacultyLessonTeachingStrategyController.listFacultyLessonTeachingStrategy(vFacultyLessonTeachingStrategy.vLessons_LessonIndex))
  }

  def createFacultyLessonTeachingStrategy(idLessons: Long) = compositeAction(NormalUser) { user => implicit template => implicit request =>
    val vFacultyLessonTeachingStrategy = new MdlFacultyLessonTeachingStrategy(You need to put the default values here setting the fixed foreign key)
    Ok(viewforms.html.formFacultyLessonTeachingStrategy(formFacultyLessonTeachingStrategy.fill(vFacultyLessonTeachingStrategy), 1))
  }

  def saveFacultyLessonTeachingStrategy(newEntry: Int) = Action { implicit request =>
  	formFacultyLessonTeachingStrategy.bindFromRequest.fold(
  	  form => {
        val errorMessage = formErrorMessage(form.errors)
        Logger.debug(errorMessage)
        BadRequest(viewforms.html.formError(errorMessage, request.headers("REFERER")))
      },
      vFacultyLessonTeachingStrategy => {
        if (vFacultyLessonTeachingStrategy.validate) {
          newEntry match {
            case 0 => SqlFacultyLessonTeachingStrategy.update(vFacultyLessonTeachingStrategy)
            case _ => SqlFacultyLessonTeachingStrategy.insert(vFacultyLessonTeachingStrategy)
          }
          Redirect(routes.FacultyLessonTeachingStrategyController.listFacultyLessonTeachingStrategy(vFacultyLessonTeachingStrategy.vLessons_LessonIndex))
        } else {
          val validationErrors = vFacultyLessonTeachingStrategy.validationErrors
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
    errMess("Error Messages:\n", formFacultyLessonTeachingStrategy.errors.toList)
  }
}
