
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

object ReferenceController extends Controller {

  val formReference = Form[MdlReference](
    mapping (
	"fidReference" -> of[Long],
	"fTitle" -> text,
	"fText" -> text,
	"fLink" -> text
    )(MdlReference.apply)(MdlReference.unapply)
  )
      

  def listReference = Action {
    Ok(viewlist.html.listReference(SqlReference.all))
  }

   def editReference(id: Long) = Action {
    Ok(viewforms.html.formReference(formReference.fill(SqlReference.select(id)), 0))
  }

   def showReference(id: Long) = Action {
    Ok(viewshow.html.showReference(SqlReference.select(id)))
  }

   def deleteReference(id: Long) = Action {
    SqlReference.delete(id)
    Ok(viewlist.html.listReference(SqlReference.all))
  }

  def createReference = Action {
    Ok(viewforms.html.formReference(formReference.fill(new MdlReference()), 1))
  }

  def saveReference(newEntry: Int) = Action { implicit request =>
  	formReference.bindFromRequest.fold(
  	  form => {
        val errorMessage = formErrorMessage(form.errors)
        Logger.debug(errorMessage)
        BadRequest(viewforms.html.formError(errorMessage, request.headers("REFERER")))
      },
      vReference => {
        if (vReference.validate) {
          newEntry match {
            case 0 => SqlReference.update(vReference)
            case _ => SqlReference.insert(vReference)
          }
          Redirect(routes.ReferenceController.listReference)
        } else {
          val validationErrors = vReference.validationErrors
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
    errMess("Error Messages:\n", formReference.errors.toList)
  }
}
