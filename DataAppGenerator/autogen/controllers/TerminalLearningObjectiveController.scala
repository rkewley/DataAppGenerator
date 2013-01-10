
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

object TerminalLearningObjectiveController extends Controller {

  val formTerminalLearningObjective = Form[MdlTerminalLearningObjective](
    mapping (
	"fidTerminalLearningObjective" -> of[Long],
	"fTerminalLearningObjective" -> text,
	"fTopic" -> of[Long],
	"fProgram" -> text
    )(MdlTerminalLearningObjective.apply)(MdlTerminalLearningObjective.unapply)
  )
      

  def listTerminalLearningObjective = Action {
    Ok(viewlist.html.listTerminalLearningObjective(SqlTerminalLearningObjective.all))
  }

   def editTerminalLearningObjective(id: Long) = Action {
    Ok(viewforms.html.formTerminalLearningObjective(formTerminalLearningObjective.fill(SqlTerminalLearningObjective.select(id)), 0))
  }

   def showTerminalLearningObjective(id: Long) = Action {
    Ok(viewshow.html.showTerminalLearningObjective(SqlTerminalLearningObjective.select(id)))
  }

   def deleteTerminalLearningObjective(id: Long) = Action {
    SqlTerminalLearningObjective.delete(id)
    Ok(viewlist.html.listTerminalLearningObjective(SqlTerminalLearningObjective.all))
  }

  def createTerminalLearningObjective = Action {
    Ok(viewforms.html.formTerminalLearningObjective(formTerminalLearningObjective.fill(new MdlTerminalLearningObjective()), 1))
  }

  def saveTerminalLearningObjective(newEntry: Int) = Action { implicit request =>
  	formTerminalLearningObjective.bindFromRequest.fold(
  	  form => {
        val errorMessage = formErrorMessage(form.errors)
        Logger.debug(errorMessage)
        BadRequest(viewforms.html.formError(errorMessage, request.headers("REFERER")))
      },
      vTerminalLearningObjective => {
        if (vTerminalLearningObjective.validate) {
          newEntry match {
            case 0 => SqlTerminalLearningObjective.update(vTerminalLearningObjective)
            case _ => SqlTerminalLearningObjective.insert(vTerminalLearningObjective)
          }
          Redirect(routes.TerminalLearningObjectiveController.listTerminalLearningObjective)
        } else {
          val validationErrors = vTerminalLearningObjective.validationErrors
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
    errMess("Error Messages:\n", formTerminalLearningObjective.errors.toList)
  }
}
