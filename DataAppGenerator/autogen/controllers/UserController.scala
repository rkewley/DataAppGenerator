
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

object UserController extends Controller {

  val formUser = Form[MdlUser](
    mapping (
	"femail" -> text,
	"fpassword" -> text,
	"fpermissions" -> text
    )(MdlUser.apply)(MdlUser.unapply)
  )
      

  def listUser = Action {
    Ok(viewlist.html.listUser(SqlUser.all))
  }

   def editUser(id: String) = Action {
    Ok(viewforms.html.formUser(formUser.fill(SqlUser.select(id)), 0))
  }

   def showUser(id: String) = Action {
    Ok(viewshow.html.showUser(SqlUser.select(id)))
  }

   def deleteUser(id: String) = Action {
    SqlUser.delete(id)
    Ok(viewlist.html.listUser(SqlUser.all))
  }

  def createUser = Action {
    Ok(viewforms.html.formUser(formUser.fill(new MdlUser()), 1))
  }

  def saveUser(newEntry: Int) = Action { implicit request =>
  	formUser.bindFromRequest.fold(
  	  form => {
        val errorMessage = formErrorMessage(form.errors)
        Logger.debug(errorMessage)
        BadRequest(viewforms.html.formError(errorMessage, request.headers("REFERER")))
      },
      vUser => {
        if (vUser.validate) {
          newEntry match {
            case 0 => SqlUser.update(vUser)
            case _ => SqlUser.insert(vUser)
          }
          Redirect(routes.UserController.listUser)
        } else {
          val validationErrors = vUser.validationErrors
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
    errMess("Error Messages:\n", formUser.errors.toList)
  }
}
