
package persistence

import models.MdlLessonLinkTopicObjectives
import anorm._
import anorm.SqlParser._
import play.api.db._
import play.api.Play.current
import play.Logger

object SqlLessonLinkTopicObjectives {

  val vLessonLinkTopicObjectives = {
    get[Long]("idLessonLinkTopicObjective") ~
	get[Long]("LessonLink") ~
	get[Long]("TopicObjective") map { case
    vidLessonLinkTopicObjective ~
		vLessonLink ~
		vTopicObjective =>
    MdlLessonLinkTopicObjectives(vidLessonLinkTopicObjective,
		vLessonLink,
		vTopicObjective)
    }
  }

  	def all: List[MdlLessonLinkTopicObjectives] = DB.withConnection { implicit c =>
  		SQL("select * from `LessonLinkTopicObjectives`").as(vLessonLinkTopicObjectives *)
	}

	def select(vidLessonLinkTopicObjective: Long): MdlLessonLinkTopicObjectives = DB.withConnection { implicit c =>
  		SQL("select * from `LessonLinkTopicObjectives` WHERE `idLessonLinkTopicObjective` = {sqlidLessonLinkTopicObjective}").on(
  			'sqlidLessonLinkTopicObjective -> vidLessonLinkTopicObjective).as(vLessonLinkTopicObjectives *).head
	}

  	def selectWhere(where: String): List[MdlLessonLinkTopicObjectives] = DB.withConnection { implicit c =>
  		SQL("select * from `LessonLinkTopicObjectives` WHERE " + where).as(vLessonLinkTopicObjectives *)
	}

	def delete(vidLessonLinkTopicObjective: Long) = DB.withConnection { implicit c =>
  		SQL("DELETE FROM `LessonLinkTopicObjectives` WHERE `idLessonLinkTopicObjective` = {sqlidLessonLinkTopicObjective}").on(
      'sqlidLessonLinkTopicObjective -> vidLessonLinkTopicObjective
  		).executeUpdate()
    }

	def insert(vLessonLinkTopicObjectives: MdlLessonLinkTopicObjectives) = DB.withConnection { implicit c =>
  		SQL("INSERT INTO `LessonLinkTopicObjectives` (`idLessonLinkTopicObjective`, `LessonLink`, `TopicObjective`) VALUES ({sqlidLessonLinkTopicObjective}, {sqlLessonLink}, {sqlTopicObjective})").on('sqlidLessonLinkTopicObjective -> vLessonLinkTopicObjectives.vidLessonLinkTopicObjective, 'sqlLessonLink -> vLessonLinkTopicObjectives.vLessonLink, 'sqlTopicObjective -> vLessonLinkTopicObjectives.vTopicObjective).executeInsert()
	}

	def update(vLessonLinkTopicObjectives: MdlLessonLinkTopicObjectives) = DB.withConnection { implicit c =>
  		SQL("UPDATE `LessonLinkTopicObjectives` SET `LessonLink` = {sqlLessonLink}, `TopicObjective` = {sqlTopicObjective} WHERE `idLessonLinkTopicObjective` = {sqlidLessonLinkTopicObjective}").on('sqlidLessonLinkTopicObjective -> vLessonLinkTopicObjectives.vidLessonLinkTopicObjective, 'sqlLessonLink -> vLessonLinkTopicObjectives.vLessonLink, 'sqlTopicObjective -> vLessonLinkTopicObjectives.vTopicObjective).executeUpdate()

  }

}