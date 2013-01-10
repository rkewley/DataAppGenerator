
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

object LinkController extends Controller {

  val formLink = Form[MdlLink](
    mapping (
	"fidLink" -> of[Long],
	"fDescription" -> text,
	"fLink" -> text
    )(MdlLink.apply)(MdlLink.unapply)
  )
      

  def listLink = Action {
    Ok(viewlist.html.listLink(SqlLink.all))
  }

   def editLink(id: Long) = Action {
    Ok(viewforms.html.formLink(formLink.fill(SqlLink.select(id)), 0))
  }

   def showLink(id: Long) = Action {
    Ok(viewshow.html.showLink(SqlLink.select(id)))
  }

   def deleteLink(id: Long) = Action {
    SqlLink.delete(id)
    Ok(viewlist.html.listLink(SqlLink.all))
  }

  def createLink = Action {
    Ok(viewforms.html.formLink(formLink.fill(new MdlLink()), 1))
  }

  def saveLink(newEntry: Int) = Action { implicit request =>
  	formLink.bindFromRequest.fold(
  	  form => {
        val errorMessage = formErrorMessage(form.errors)
        Logger.debug(errorMessage)
        BadRequest(viewforms.html.formError(errorMessage, request.headers("REFERER")))
      },
      vLink => {
        if (vLink.validate) {
          newEntry match {
            case 0 => SqlLink.update(vLink)
            case _ => SqlLink.insert(vLink)
          }
          Redirect(routes.LinkController.listLink)
        } else {
          val validationErrors = vLink.validationErrors
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
    errMess("Error Messages:\n", formLink.errors.toList)
  }
}
