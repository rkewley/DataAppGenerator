
package persistence

import models.MdlReference
import anorm._
import anorm.SqlParser._
import play.api.db._
import play.api.Play.current
import play.Logger

object SqlReference {

  val vReference = {
    get[Long]("idReference") ~
	get[String]("Title") ~
	get[String]("Text") ~
	get[String]("Link") map { case
    vidReference ~
		vTitle ~
		vText ~
		vLink =>
    MdlReference(vidReference,
		vTitle,
		vText,
		vLink)
    }
  }

  	def all: List[MdlReference] = DB.withConnection { implicit c =>
  		SQL("select * from `Reference`").as(vReference *)
	}

	def select(vidReference: Long): MdlReference = DB.withConnection { implicit c =>
  		SQL("select * from `Reference` WHERE `idReference` = {sqlidReference}").on(
  			'sqlidReference -> vidReference).as(vReference *).head
	}

  	def selectWhere(where: String): List[MdlReference] = DB.withConnection { implicit c =>
  		SQL("select * from `Reference` WHERE " + where).as(vReference *)
	}

	def delete(vidReference: Long) = DB.withConnection { implicit c =>
  		SQL("DELETE FROM `Reference` WHERE `idReference` = {sqlidReference}").on(
      'sqlidReference -> vidReference
  		).executeUpdate()
    }

	def insert(vReference: MdlReference) = DB.withConnection { implicit c =>
  		SQL("INSERT INTO `Reference` (`Title`, `Text`, `Link`) VALUES ({sqlTitle}, {sqlText}, {sqlLink})").on('sqlTitle -> vReference.vTitle, 'sqlText -> vReference.vText, 'sqlLink -> vReference.vLink).executeInsert()
	}

	def update(vReference: MdlReference) = DB.withConnection { implicit c =>
  		SQL("UPDATE `Reference` SET `Title` = {sqlTitle}, `Text` = {sqlText}, `Link` = {sqlLink} WHERE `idReference` = {sqlidReference}").on('sqlidReference -> vReference.vidReference, 'sqlTitle -> vReference.vTitle, 'sqlText -> vReference.vText, 'sqlLink -> vReference.vLink).executeUpdate()

  }

}