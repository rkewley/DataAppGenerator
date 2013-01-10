
package persistence

import models.MdlTopicLink
import anorm._
import anorm.SqlParser._
import play.api.db._
import play.api.Play.current
import play.Logger

object SqlTopicLink {

  val vTopicLink = {
    get[Long]("idTopicLink") ~
	get[Long]("Topic") ~
	get[String]("Link") ~
	get[String]("Description") map { case
    vidTopicLink ~
		vTopic ~
		vLink ~
		vDescription =>
    MdlTopicLink(vidTopicLink,
		vTopic,
		vLink,
		vDescription)
    }
  }

  	def all: List[MdlTopicLink] = DB.withConnection { implicit c =>
  		SQL("select * from `TopicLink`").as(vTopicLink *)
	}

	def select(vidTopicLink: Long): MdlTopicLink = DB.withConnection { implicit c =>
  		SQL("select * from `TopicLink` WHERE `idTopicLink` = {sqlidTopicLink}").on(
  			'sqlidTopicLink -> vidTopicLink).as(vTopicLink *).head
	}

  	def selectWhere(where: String): List[MdlTopicLink] = DB.withConnection { implicit c =>
  		SQL("select * from `TopicLink` WHERE " + where).as(vTopicLink *)
	}

	def delete(vidTopicLink: Long) = DB.withConnection { implicit c =>
  		SQL("DELETE FROM `TopicLink` WHERE `idTopicLink` = {sqlidTopicLink}").on(
      'sqlidTopicLink -> vidTopicLink
  		).executeUpdate()
    }

	def insert(vTopicLink: MdlTopicLink) = DB.withConnection { implicit c =>
  		SQL("INSERT INTO `TopicLink` (`idTopicLink`, `Topic`, `Link`, `Description`) VALUES ({sqlidTopicLink}, {sqlTopic}, {sqlLink}, {sqlDescription})").on('sqlidTopicLink -> vTopicLink.vidTopicLink, 'sqlTopic -> vTopicLink.vTopic, 'sqlLink -> vTopicLink.vLink, 'sqlDescription -> vTopicLink.vDescription).executeInsert()
	}

	def update(vTopicLink: MdlTopicLink) = DB.withConnection { implicit c =>
  		SQL("UPDATE `TopicLink` SET `Topic` = {sqlTopic}, `Link` = {sqlLink}, `Description` = {sqlDescription} WHERE `idTopicLink` = {sqlidTopicLink}").on('sqlidTopicLink -> vTopicLink.vidTopicLink, 'sqlTopic -> vTopicLink.vTopic, 'sqlLink -> vTopicLink.vLink, 'sqlDescription -> vTopicLink.vDescription).executeUpdate()

  }

}