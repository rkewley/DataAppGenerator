
package persistence

import models.MdlCourseReferences
import anorm._
import anorm.SqlParser._
import play.api.db._
import play.api.Play.current
import play.Logger

object SqlCourseReferences {

  val vCourseReferences = {
    get[Long]("idCourseReferences") ~
	get[Long]("Course") ~
	get[Long]("Reference") map { case
    vidCourseReferences ~
		vCourse ~
		vReference =>
    MdlCourseReferences(vidCourseReferences,
		vCourse,
		vReference)
    }
  }

  	def all: List[MdlCourseReferences] = DB.withConnection { implicit c =>
  		SQL("select * from `CourseReferences`").as(vCourseReferences *)
	}

	def select(vidCourseReferences: Long): MdlCourseReferences = DB.withConnection { implicit c =>
  		SQL("select * from `CourseReferences` WHERE `idCourseReferences` = {sqlidCourseReferences}").on(
  			'sqlidCourseReferences -> vidCourseReferences).as(vCourseReferences *).head
	}

  	def selectWhere(where: String): List[MdlCourseReferences] = DB.withConnection { implicit c =>
  		SQL("select * from `CourseReferences` WHERE " + where).as(vCourseReferences *)
	}

	def delete(vidCourseReferences: Long) = DB.withConnection { implicit c =>
  		SQL("DELETE FROM `CourseReferences` WHERE `idCourseReferences` = {sqlidCourseReferences}").on(
      'sqlidCourseReferences -> vidCourseReferences
  		).executeUpdate()
    }

	def insert(vCourseReferences: MdlCourseReferences) = DB.withConnection { implicit c =>
  		SQL("INSERT INTO `CourseReferences` (`Course`, `Reference`) VALUES ({sqlCourse}, {sqlReference})").on('sqlCourse -> vCourseReferences.vCourse, 'sqlReference -> vCourseReferences.vReference).executeInsert()
	}

	def update(vCourseReferences: MdlCourseReferences) = DB.withConnection { implicit c =>
  		SQL("UPDATE `CourseReferences` SET `Course` = {sqlCourse}, `Reference` = {sqlReference} WHERE `idCourseReferences` = {sqlidCourseReferences}").on('sqlidCourseReferences -> vCourseReferences.vidCourseReferences, 'sqlCourse -> vCourseReferences.vCourse, 'sqlReference -> vCourseReferences.vReference).executeUpdate()

  }

}