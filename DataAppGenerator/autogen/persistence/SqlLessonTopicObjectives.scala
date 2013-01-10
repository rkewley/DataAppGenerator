
package persistence

import models.MdlLessonTopicObjectives
import anorm._
import anorm.SqlParser._
import play.api.db._
import play.api.Play.current
import play.Logger

object SqlLessonTopicObjectives {

  val vLessonTopicObjectives = {
    get[Long]("idLessonTopicObjectives") ~
	get[Long]("Lesson") ~
	get[Long]("TopicObjective") map { case
    vidLessonTopicObjectives ~
		vLesson ~
		vTopicObjective =>
    MdlLessonTopicObjectives(vidLessonTopicObjectives,
		vLesson,
		vTopicObjective)
    }
  }

  	def all: List[MdlLessonTopicObjectives] = DB.withConnection { implicit c =>
  		SQL("select * from `LessonTopicObjectives`").as(vLessonTopicObjectives *)
	}

	def select(vidLessonTopicObjectives: Long): MdlLessonTopicObjectives = DB.withConnection { implicit c =>
  		SQL("select * from `LessonTopicObjectives` WHERE `idLessonTopicObjectives` = {sqlidLessonTopicObjectives}").on(
  			'sqlidLessonTopicObjectives -> vidLessonTopicObjectives).as(vLessonTopicObjectives *).head
	}

  	def selectWhere(where: String): List[MdlLessonTopicObjectives] = DB.withConnection { implicit c =>
  		SQL("select * from `LessonTopicObjectives` WHERE " + where).as(vLessonTopicObjectives *)
	}

	def delete(vidLessonTopicObjectives: Long) = DB.withConnection { implicit c =>
  		SQL("DELETE FROM `LessonTopicObjectives` WHERE `idLessonTopicObjectives` = {sqlidLessonTopicObjectives}").on(
      'sqlidLessonTopicObjectives -> vidLessonTopicObjectives
  		).executeUpdate()
    }

	def insert(vLessonTopicObjectives: MdlLessonTopicObjectives) = DB.withConnection { implicit c =>
  		SQL("INSERT INTO `LessonTopicObjectives` (`Lesson`, `TopicObjective`) VALUES ({sqlLesson}, {sqlTopicObjective})").on('sqlLesson -> vLessonTopicObjectives.vLesson, 'sqlTopicObjective -> vLessonTopicObjectives.vTopicObjective).executeInsert()
	}

	def update(vLessonTopicObjectives: MdlLessonTopicObjectives) = DB.withConnection { implicit c =>
  		SQL("UPDATE `LessonTopicObjectives` SET `Lesson` = {sqlLesson}, `TopicObjective` = {sqlTopicObjective} WHERE `idLessonTopicObjectives` = {sqlidLessonTopicObjectives}").on('sqlidLessonTopicObjectives -> vLessonTopicObjectives.vidLessonTopicObjectives, 'sqlLesson -> vLessonTopicObjectives.vLesson, 'sqlTopicObjective -> vLessonTopicObjectives.vTopicObjective).executeUpdate()

  }

}