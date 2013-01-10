
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

object LessonLinksController extends Base {

  val formLessonLinks = Form[MdlLessonLinks](
    mapping (
	"fLessonLinkNumber" -> of[Long],
	"fLink" -> text,
	"fDescription" -> text,
	"fIsFileLiink" -> of[Boolean],
	"fLesson" -> of[Long]
    )(MdlLessonLinks.apply)(MdlLessonLinks.unapply)
  )
      

  def listLessonLinks(idLessons: Long) = Action {
     Ok(viewlist.html.listLessonLinks(SqlLessonLinks.selectWhere("`Lesson` = " + idLessons), idLessons))
  }

   def editLessonLinks(id: Long) = compositeAction(NormalUser) { user => implicit template => implicit request =>
    Ok(viewforms.html.formLessonLinks(formLessonLinks.fill(SqlLessonLinks.select(id)), 0))
  }

   def showLessonLinks(id: Long) = Action {
    Ok(viewshow.html.showLessonLinks(SqlLessonLinks.select(id)))
  }

   def deleteLessonLinks(id: Long) = compositeAction(NormalUser) { user => implicit template => implicit request =>
    val vLessonLinks = SqlLessonLinks.select(id)
    SqlLessonLinks.delete(id)
    Redirect(routes.LessonLinksController.listLessonLinks(vLessonLinks.vLesson))
  }

  def createLessonLinks(idLessons: Long) = compositeAction(NormalUser) { user => implicit template => implicit request =>
    val vLessonLinks = new MdlLessonLinks(You need to put the default values here setting the fixed foreign key)
    Ok(viewforms.html.formLessonLinks(formLessonLinks.fill(vLessonLinks), 1))
  }

  def saveLessonLinks(newEntry: Int) = Action { implicit request =>
  	formLessonLinks.bindFromRequest.fold(
  	  form => {
        val errorMessage = formErrorMessage(form.errors)
        Logger.debug(errorMessage)
        BadRequest(viewforms.html.formError(errorMessage, request.headers("REFERER")))
      },
      vLessonLinks => {
        if (vLessonLinks.validate) {
          newEntry match {
            case 0 => SqlLessonLinks.update(vLessonLinks)
            case _ => SqlLessonLinks.insert(vLessonLinks)
          }
          Redirect(routes.LessonLinksController.listLessonLinks(vLessonLinks.vLesson))
        } else {
          val validationErrors = vLessonLinks.validationErrors
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
    errMess("Error Messages:\n", formLessonLinks.errors.toList)
  }
}
