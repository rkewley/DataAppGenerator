
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

object TopicLinkController extends Controller {

  val formTopicLink = Form[MdlTopicLink](
    mapping (
	"fidTopicLink" -> of[Long],
	"fTopic" -> of[Long],
	"fLink" -> text,
	"fDescription" -> text
    )(MdlTopicLink.apply)(MdlTopicLink.unapply)
  )
      

  def listTopicLink = Action {
    Ok(viewlist.html.listTopicLink(SqlTopicLink.all))
  }

   def editTopicLink(id: Long) = Action {
    Ok(viewforms.html.formTopicLink(formTopicLink.fill(SqlTopicLink.select(id)), 0))
  }

   def showTopicLink(id: Long) = Action {
    Ok(viewshow.html.showTopicLink(SqlTopicLink.select(id)))
  }

   def deleteTopicLink(id: Long) = Action {
    SqlTopicLink.delete(id)
    Ok(viewlist.html.listTopicLink(SqlTopicLink.all))
  }

  def createTopicLink = Action {
    Ok(viewforms.html.formTopicLink(formTopicLink.fill(new MdlTopicLink()), 1))
  }

  def saveTopicLink(newEntry: Int) = Action { implicit request =>
  	formTopicLink.bindFromRequest.fold(
  	  form => {
        val errorMessage = formErrorMessage(form.errors)
        Logger.debug(errorMessage)
        BadRequest(viewforms.html.formError(errorMessage, request.headers("REFERER")))
      },
      vTopicLink => {
        if (vTopicLink.validate) {
          newEntry match {
            case 0 => SqlTopicLink.update(vTopicLink)
            case _ => SqlTopicLink.insert(vTopicLink)
          }
          Redirect(routes.TopicLinkController.listTopicLink)
        } else {
          val validationErrors = vTopicLink.validationErrors
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
    errMess("Error Messages:\n", formTopicLink.errors.toList)
  }
}
