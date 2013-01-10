
package persistence

import models.MdlFacultyLessonTeachingStrategy
import anorm._
import anorm.SqlParser._
import play.api.db._
import play.api.Play.current
import play.Logger

object SqlFacultyLessonTeachingStrategy {

  val vFacultyLessonTeachingStrategy = {
    get[String]("Faculty_Email") ~
	get[Long]("Lessons_LessonIndex") ~
	get[String]("TeachingStrategy") map { case
    vFaculty_Email ~
		vLessons_LessonIndex ~
		vTeachingStrategy =>
    MdlFacultyLessonTeachingStrategy(vFaculty_Email,
		vLessons_LessonIndex,
		vTeachingStrategy)
    }
  }

  	def all: List[MdlFacultyLessonTeachingStrategy] = DB.withConnection { implicit c =>
  		SQL("select * from `FacultyLessonTeachingStrategy`").as(vFacultyLessonTeachingStrategy *)
	}

	def select(vFaculty_Email: String, vLessons_LessonIndex: Long): MdlFacultyLessonTeachingStrategy = DB.withConnection { implicit c =>
  		SQL("select * from `FacultyLessonTeachingStrategy` WHERE `Faculty_Email` = {sqlFaculty_Email} AND `Lessons_LessonIndex` = {sqlLessons_LessonIndex}").on(
  			'sqlFaculty_Email -> vFaculty_Email, 'sqlLessons_LessonIndex -> vLessons_LessonIndex).as(vFacultyLessonTeachingStrategy *).head
	}

  	def selectWhere(where: String): List[MdlFacultyLessonTeachingStrategy] = DB.withConnection { implicit c =>
  		SQL("select * from `FacultyLessonTeachingStrategy` WHERE " + where).as(vFacultyLessonTeachingStrategy *)
	}

	def delete(vFaculty_Email: String, vLessons_LessonIndex: Long) = DB.withConnection { implicit c =>
  		SQL("DELETE FROM `FacultyLessonTeachingStrategy` WHERE `Faculty_Email` = {sqlFaculty_Email} AND `Lessons_LessonIndex` = {sqlLessons_LessonIndex}").on(
      'sqlFaculty_Email -> vFaculty_Email, 'sqlLessons_LessonIndex -> vLessons_LessonIndex
  		).executeUpdate()
    }

	def insert(vFacultyLessonTeachingStrategy: MdlFacultyLessonTeachingStrategy) = DB.withConnection { implicit c =>
  		SQL("INSERT INTO `FacultyLessonTeachingStrategy` (`Faculty_Email`, `Lessons_LessonIndex`, `TeachingStrategy`) VALUES ({sqlFaculty_Email}, {sqlLessons_LessonIndex}, {sqlTeachingStrategy})").on('sqlFaculty_Email -> vFacultyLessonTeachingStrategy.vFaculty_Email, 'sqlLessons_LessonIndex -> vFacultyLessonTeachingStrategy.vLessons_LessonIndex, 'sqlTeachingStrategy -> vFacultyLessonTeachingStrategy.vTeachingStrategy).executeInsert()
	}

	def update(vFacultyLessonTeachingStrategy: MdlFacultyLessonTeachingStrategy) = DB.withConnection { implicit c =>
  		SQL("UPDATE `FacultyLessonTeachingStrategy` SET `TeachingStrategy` = {sqlTeachingStrategy} WHERE `Faculty_Email` = {sqlFaculty_Email} AND `Lessons_LessonIndex` = {sqlLessons_LessonIndex}").on('sqlFaculty_Email -> vFacultyLessonTeachingStrategy.vFaculty_Email, 'sqlLessons_LessonIndex -> vFacultyLessonTeachingStrategy.vLessons_LessonIndex, 'sqlTeachingStrategy -> vFacultyLessonTeachingStrategy.vTeachingStrategy).executeUpdate()

  }

}