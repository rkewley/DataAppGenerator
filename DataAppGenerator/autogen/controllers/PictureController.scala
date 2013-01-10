
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

object PictureController extends Controller {

  val formPicture = Form[MdlPicture](
    mapping (
	"fidPicture" -> of[Long],
	"fHyperlink" -> text,
	"fAlternateText" -> text,
	"fCaption" -> text
    )(MdlPicture.apply)(MdlPicture.unapply)
  )
      

  def listPicture = Action {
    Ok(viewlist.html.listPicture(SqlPicture.all))
  }

   def editPicture(id: Long) = Action {
    Ok(viewforms.html.formPicture(formPicture.fill(SqlPicture.select(id)), 0))
  }

   def showPicture(id: Long) = Action {
    Ok(viewshow.html.showPicture(SqlPicture.select(id)))
  }

   def deletePicture(id: Long) = Action {
    SqlPicture.delete(id)
    Ok(viewlist.html.listPicture(SqlPicture.all))
  }

  def createPicture = Action {
    Ok(viewforms.html.formPicture(formPicture.fill(new MdlPicture()), 1))
  }

  def savePicture(newEntry: Int) = Action { implicit request =>
  	formPicture.bindFromRequest.fold(
  	  form => {
        val errorMessage = formErrorMessage(form.errors)
        Logger.debug(errorMessage)
        BadRequest(viewforms.html.formError(errorMessage, request.headers("REFERER")))
      },
      vPicture => {
        if (vPicture.validate) {
          newEntry match {
            case 0 => SqlPicture.update(vPicture)
            case _ => SqlPicture.insert(vPicture)
          }
          Redirect(routes.PictureController.listPicture)
        } else {
          val validationErrors = vPicture.validationErrors
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
    errMess("Error Messages:\n", formPicture.errors.toList)
  }
}
