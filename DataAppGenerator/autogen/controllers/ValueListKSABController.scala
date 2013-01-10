
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

object ValueListKSABController extends Controller {

  val formValueListKSAB = Form[MdlValueListKSAB](
    mapping (
	"fKSAB" -> text
    )(MdlValueListKSAB.apply)(MdlValueListKSAB.unapply)
  )
      

  def listValueListKSAB = Action {
    Ok(viewlist.html.listValueListKSAB(SqlValueListKSAB.all))
  }

   def editValueListKSAB(id: String) = Action {
    Ok(viewforms.html.formValueListKSAB(formValueListKSAB.fill(SqlValueListKSAB.select(id)), 0))
  }

   def showValueListKSAB(id: String) = Action {
    Ok(viewshow.html.showValueListKSAB(SqlValueListKSAB.select(id)))
  }

   def deleteValueListKSAB(id: String) = Action {
    SqlValueListKSAB.delete(id)
    Ok(viewlist.html.listValueListKSAB(SqlValueListKSAB.all))
  }

  def createValueListKSAB = Action {
    Ok(viewforms.html.formValueListKSAB(formValueListKSAB.fill(new MdlValueListKSAB()), 1))
  }

  def saveValueListKSAB(newEntry: Int) = Action { implicit request =>
  	formValueListKSAB.bindFromRequest.fold(
  	  form => {
        val errorMessage = formErrorMessage(form.errors)
        Logger.debug(errorMessage)
        BadRequest(viewforms.html.formError(errorMessage, request.headers("REFERER")))
      },
      vValueListKSAB => {
        if (vValueListKSAB.validate) {
          newEntry match {
            case 0 => SqlValueListKSAB.update(vValueListKSAB)
            case _ => SqlValueListKSAB.insert(vValueListKSAB)
          }
          Redirect(routes.ValueListKSABController.listValueListKSAB)
        } else {
          val validationErrors = vValueListKSAB.validationErrors
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
    errMess("Error Messages:\n", formValueListKSAB.errors.toList)
  }
}
