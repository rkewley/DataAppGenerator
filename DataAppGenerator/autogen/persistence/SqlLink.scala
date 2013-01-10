
package persistence

import models.MdlLink
import anorm._
import anorm.SqlParser._
import play.api.db._
import play.api.Play.current
import play.Logger

object SqlLink {

  val vLink = {
    get[Long]("idLink") ~
	get[String]("Description") ~
	get[String]("Link") map { case
    vidLink ~
		vDescription ~
		vLink =>
    MdlLink(vidLink,
		vDescription,
		vLink)
    }
  }

  	def all: List[MdlLink] = DB.withConnection { implicit c =>
  		SQL("select * from `Link`").as(vLink *)
	}

	def select(vidLink: Long): MdlLink = DB.withConnection { implicit c =>
  		SQL("select * from `Link` WHERE `idLink` = {sqlidLink}").on(
  			'sqlidLink -> vidLink).as(vLink *).head
	}

  	def selectWhere(where: String): List[MdlLink] = DB.withConnection { implicit c =>
  		SQL("select * from `Link` WHERE " + where).as(vLink *)
	}

	def delete(vidLink: Long) = DB.withConnection { implicit c =>
  		SQL("DELETE FROM `Link` WHERE `idLink` = {sqlidLink}").on(
      'sqlidLink -> vidLink
  		).executeUpdate()
    }

	def insert(vLink: MdlLink) = DB.withConnection { implicit c =>
  		SQL("INSERT INTO `Link` (`Description`, `Link`) VALUES ({sqlDescription}, {sqlLink})").on('sqlDescription -> vLink.vDescription, 'sqlLink -> vLink.vLink).executeInsert()
	}

	def update(vLink: MdlLink) = DB.withConnection { implicit c =>
  		SQL("UPDATE `Link` SET `Description` = {sqlDescription}, `Link` = {sqlLink} WHERE `idLink` = {sqlidLink}").on('sqlidLink -> vLink.vidLink, 'sqlDescription -> vLink.vDescription, 'sqlLink -> vLink.vLink).executeUpdate()

  }

}