
package persistence

import models.MdlPicture
import anorm._
import anorm.SqlParser._
import play.api.db._
import play.api.Play.current
import play.Logger

object SqlPicture {

  val vPicture = {
    get[Long]("idPicture") ~
	get[String]("Hyperlink") ~
	get[String]("AlternateText") ~
	get[String]("Caption") map { case
    vidPicture ~
		vHyperlink ~
		vAlternateText ~
		vCaption =>
    MdlPicture(vidPicture,
		vHyperlink,
		vAlternateText,
		vCaption)
    }
  }

  	def all: List[MdlPicture] = DB.withConnection { implicit c =>
  		SQL("select * from `Picture`").as(vPicture *)
	}

	def select(vidPicture: Long): MdlPicture = DB.withConnection { implicit c =>
  		SQL("select * from `Picture` WHERE `idPicture` = {sqlidPicture}").on(
  			'sqlidPicture -> vidPicture).as(vPicture *).head
	}

  	def selectWhere(where: String): List[MdlPicture] = DB.withConnection { implicit c =>
  		SQL("select * from `Picture` WHERE " + where).as(vPicture *)
	}

	def delete(vidPicture: Long) = DB.withConnection { implicit c =>
  		SQL("DELETE FROM `Picture` WHERE `idPicture` = {sqlidPicture}").on(
      'sqlidPicture -> vidPicture
  		).executeUpdate()
    }

	def insert(vPicture: MdlPicture) = DB.withConnection { implicit c =>
  		SQL("INSERT INTO `Picture` (`Hyperlink`, `AlternateText`, `Caption`) VALUES ({sqlHyperlink}, {sqlAlternateText}, {sqlCaption})").on('sqlHyperlink -> vPicture.vHyperlink, 'sqlAlternateText -> vPicture.vAlternateText, 'sqlCaption -> vPicture.vCaption).executeInsert()
	}

	def update(vPicture: MdlPicture) = DB.withConnection { implicit c =>
  		SQL("UPDATE `Picture` SET `Hyperlink` = {sqlHyperlink}, `AlternateText` = {sqlAlternateText}, `Caption` = {sqlCaption} WHERE `idPicture` = {sqlidPicture}").on('sqlidPicture -> vPicture.vidPicture, 'sqlHyperlink -> vPicture.vHyperlink, 'sqlAlternateText -> vPicture.vAlternateText, 'sqlCaption -> vPicture.vCaption).executeUpdate()

  }

}