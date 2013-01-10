
package persistence

import models.MdlTopicReferences
import anorm._
import anorm.SqlParser._
import play.api.db._
import play.api.Play.current
import play.Logger

object SqlTopicReferences {

  val vTopicReferences = {
    get[Long]("idTopicReferences") ~
	get[Long]("Topic") ~
	get[Long]("Reference") map { case
    vidTopicReferences ~
		vTopic ~
		vReference =>
    MdlTopicReferences(vidTopicReferences,
		vTopic,
		vReference)
    }
  }

  	def all: List[MdlTopicReferences] = DB.withConnection { implicit c =>
  		SQL("select * from `TopicReferences`").as(vTopicReferences *)
	}

	def select(vidTopicReferences: Long): MdlTopicReferences = DB.withConnection { implicit c =>
  		SQL("select * from `TopicReferences` WHERE `idTopicReferences` = {sqlidTopicReferences}").on(
  			'sqlidTopicReferences -> vidTopicReferences).as(vTopicReferences *).head
	}

  	def selectWhere(where: String): List[MdlTopicReferences] = DB.withConnection { implicit c =>
  		SQL("select * from `TopicReferences` WHERE " + where).as(vTopicReferences *)
	}

	def delete(vidTopicReferences: Long) = DB.withConnection { implicit c =>
  		SQL("DELETE FROM `TopicReferences` WHERE `idTopicReferences` = {sqlidTopicReferences}").on(
      'sqlidTopicReferences -> vidTopicReferences
  		).executeUpdate()
    }

	def insert(vTopicReferences: MdlTopicReferences) = DB.withConnection { implicit c =>
  		SQL("INSERT INTO `TopicReferences` (`Topic`, `Reference`) VALUES ({sqlTopic}, {sqlReference})").on('sqlTopic -> vTopicReferences.vTopic, 'sqlReference -> vTopicReferences.vReference).executeInsert()
	}

	def update(vTopicReferences: MdlTopicReferences) = DB.withConnection { implicit c =>
  		SQL("UPDATE `TopicReferences` SET `Topic` = {sqlTopic}, `Reference` = {sqlReference} WHERE `idTopicReferences` = {sqlidTopicReferences}").on('sqlidTopicReferences -> vTopicReferences.vidTopicReferences, 'sqlTopic -> vTopicReferences.vTopic, 'sqlReference -> vTopicReferences.vReference).executeUpdate()

  }

}