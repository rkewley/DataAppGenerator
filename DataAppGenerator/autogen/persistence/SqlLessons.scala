
package persistence

import models.MdlLessons
import anorm._
import anorm.SqlParser._
import play.api.db._
import play.api.Play.current
import play.Logger

object SqlLessons {

  val vLessons = {
    get[Long]("LessonIndex") ~
	get[Long]("LessonNumber") ~
	get[String]("LessonName") ~
	get[String]("Assignment") ~
	get[String]("Location") ~
	get[Long]("idCourse") ~
	get[Long]("Duration") ~
	get[Boolean]("Lab") ~
	get[String]("LessonSummary") map { case
    vLessonIndex ~
		vLessonNumber ~
		vLessonName ~
		vAssignment ~
		vLocation ~
		vidCourse ~
		vDuration ~
		vLab ~
		vLessonSummary =>
    MdlLessons(vLessonIndex,
		vLessonNumber,
		vLessonName,
		vAssignment,
		vLocation,
		vidCourse,
		vDuration,
		vLab,
		vLessonSummary)
    }
  }

  	def all: List[MdlLessons] = DB.withConnection { implicit c =>
  		SQL("select * from `Lessons`").as(vLessons *)
	}

	def select(vLessonIndex: Long): MdlLessons = DB.withConnection { implicit c =>
  		SQL("select * from `Lessons` WHERE `LessonIndex` = {sqlLessonIndex}").on(
  			'sqlLessonIndex -> vLessonIndex).as(vLessons *).head
	}

  	def selectWhere(where: String): List[MdlLessons] = DB.withConnection { implicit c =>
  		SQL("select * from `Lessons` WHERE " + where).as(vLessons *)
	}

	def delete(vLessonIndex: Long) = DB.withConnection { implicit c =>
  		SQL("DELETE FROM `Lessons` WHERE `LessonIndex` = {sqlLessonIndex}").on(
      'sqlLessonIndex -> vLessonIndex
  		).executeUpdate()
    }

	def insert(vLessons: MdlLessons) = DB.withConnection { implicit c =>
  		SQL("INSERT INTO `Lessons` (`LessonNumber`, `LessonName`, `Assignment`, `Location`, `idCourse`, `Duration`, `Lab`, `LessonSummary`) VALUES ({sqlLessonNumber}, {sqlLessonName}, {sqlAssignment}, {sqlLocation}, {sqlidCourse}, {sqlDuration}, {sqlLab}, {sqlLessonSummary})").on('sqlLessonNumber -> vLessons.vLessonNumber, 'sqlLessonName -> vLessons.vLessonName, 'sqlAssignment -> vLessons.vAssignment, 'sqlLocation -> vLessons.vLocation, 'sqlidCourse -> vLessons.vidCourse, 'sqlDuration -> vLessons.vDuration, 'sqlLab -> vLessons.vLab, 'sqlLessonSummary -> vLessons.vLessonSummary).executeInsert()
	}

	def update(vLessons: MdlLessons) = DB.withConnection { implicit c =>
  		SQL("UPDATE `Lessons` SET `LessonNumber` = {sqlLessonNumber}, `LessonName` = {sqlLessonName}, `Assignment` = {sqlAssignment}, `Location` = {sqlLocation}, `idCourse` = {sqlidCourse}, `Duration` = {sqlDuration}, `Lab` = {sqlLab}, `LessonSummary` = {sqlLessonSummary} WHERE `LessonIndex` = {sqlLessonIndex}").on('sqlLessonIndex -> vLessons.vLessonIndex, 'sqlLessonNumber -> vLessons.vLessonNumber, 'sqlLessonName -> vLessons.vLessonName, 'sqlAssignment -> vLessons.vAssignment, 'sqlLocation -> vLessons.vLocation, 'sqlidCourse -> vLessons.vidCourse, 'sqlDuration -> vLessons.vDuration, 'sqlLab -> vLessons.vLab, 'sqlLessonSummary -> vLessons.vLessonSummary).executeUpdate()

  }

}