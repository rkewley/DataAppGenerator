
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

object LessonTopicObjectivesController extends Base {

  val formLessonTopicObjectives = Form[MdlLessonTopicObjectives](
    mapping (
	"fidLessonTopicObjectives" -> of[Long],
	"fLesson" -> of[Long],
	"fTopicObjective" -> of[Long]
    )(MdlLessonTopicObjectives.apply)(MdlLessonTopicObjectives.unapply)
  )
      

  def listLessonTopicObjectives(idLessons: Long) = Action {
     Ok(viewlist.html.listLessonTopicObjectives(SqlLessonTopicObjectives.selectWhere("`Lesson` = " + idLessons), idLessons))
  }

   def editLessonTopicObjectives(id: Long) = compositeAction(NormalUser) { user => implicit template => implicit request =>
    Ok(viewforms.html.formLessonTopicObjectives(formLessonTopicObjectives.fill(SqlLessonTopicObjectives.select(id)), 0))
  }

   def showLessonTopicObjectives(id: Long) = Action {
    Ok(viewshow.html.showLessonTopicObjectives(SqlLessonTopicObjectives.select(id)))
  }

   def deleteLessonTopicObjectives(id: Long) = compositeAction(NormalUser) { user => implicit template => implicit request =>
    val vLessonTopicObjectives = SqlLessonTopicObjectives.select(id)
    SqlLessonTopicObjectives.delete(id)
    Redirect(routes.LessonTopicObjectivesController.listLessonTopicObjectives(vLessonTopicObjectives.vLesson))
  }

  def createLessonTopicObjectives(idLessons: Long) = compositeAction(NormalUser) { user => implicit template => implicit request =>
    val vLessonTopicObjectives = new MdlLessonTopicObjectives(You need to put the default values here setting the fixed foreign key)
    Ok(viewforms.html.formLessonTopicObjectives(formLessonTopicObjectives.fill(vLessonTopicObjectives), 1))
  }

  def saveLessonTopicObjectives(newEntry: Int) = Action { implicit request =>
  	formLessonTopicObjectives.bindFromRequest.fold(
  	  form => {
        val errorMessage = formErrorMessage(form.errors)
        Logger.debug(errorMessage)
        BadRequest(viewforms.html.formError(errorMessage, request.headers("REFERER")))
      },
      vLessonTopicObjectives => {
        if (vLessonTopicObjectives.validate) {
          newEntry match {
            case 0 => SqlLessonTopicObjectives.update(vLessonTopicObjectives)
            case _ => SqlLessonTopicObjectives.insert(vLessonTopicObjectives)
          }
          Redirect(routes.LessonTopicObjectivesController.listLessonTopicObjectives(vLessonTopicObjectives.vLesson))
        } else {
          val validationErrors = vLessonTopicObjectives.validationErrors
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
    errMess("Error Messages:\n", formLessonTopicObjectives.errors.toList)
  }
}
