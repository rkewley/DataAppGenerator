
package persistence

import models.MdlCourses
import anorm._
import anorm.SqlParser._
import play.api.db._
import play.api.Play.current
import play.Logger

object SqlCourses {

  val vCourses = {
    get[Long]("idCourse") ~
	get[String]("CourseIDNumber") ~
	get[Long]("AcademicYear") ~
	get[Long]("AcademicTerm") ~
	get[String]("CourseName") ~
	get[String]("CourseDirectorEmail") ~
	get[String]("ProgramDirectorEmail") ~
	get[String]("CourseDescriptionRedbook") ~
	get[Double]("CreditHours") ~
	get[String]("Prerequisites") ~
	get[String]("Corequisites") ~
	get[String]("Disqualifiers") ~
	get[String]("CourseStrategy") ~
	get[String]("CriteriaForPassing") ~
	get[String]("AdminInstructions") ~
	get[Long]("DepartmentID") ~
	get[Boolean]("CourseWebsite") ~
	get[String]("CourseDescriptionWebsite") map { case
    vidCourse ~
		vCourseIDNumber ~
		vAcademicYear ~
		vAcademicTerm ~
		vCourseName ~
		vCourseDirectorEmail ~
		vProgramDirectorEmail ~
		vCourseDescriptionRedbook ~
		vCreditHours ~
		vPrerequisites ~
		vCorequisites ~
		vDisqualifiers ~
		vCourseStrategy ~
		vCriteriaForPassing ~
		vAdminInstructions ~
		vDepartmentID ~
		vCourseWebsite ~
		vCourseDescriptionWebsite =>
    MdlCourses(vidCourse,
		vCourseIDNumber,
		vAcademicYear,
		vAcademicTerm,
		vCourseName,
		vCourseDirectorEmail,
		vProgramDirectorEmail,
		vCourseDescriptionRedbook,
		vCreditHours,
		vPrerequisites,
		vCorequisites,
		vDisqualifiers,
		vCourseStrategy,
		vCriteriaForPassing,
		vAdminInstructions,
		vDepartmentID,
		vCourseWebsite,
		vCourseDescriptionWebsite)
    }
  }

  	def all: List[MdlCourses] = DB.withConnection { implicit c =>
  		SQL("select * from `Courses`").as(vCourses *)
	}

	def select(vidCourse: Long): MdlCourses = DB.withConnection { implicit c =>
  		SQL("select * from `Courses` WHERE `idCourse` = {sqlidCourse}").on(
  			'sqlidCourse -> vidCourse).as(vCourses *).head
	}

  	def selectWhere(where: String): List[MdlCourses] = DB.withConnection { implicit c =>
  		SQL("select * from `Courses` WHERE " + where).as(vCourses *)
	}

	def delete(vidCourse: Long) = DB.withConnection { implicit c =>
  		SQL("DELETE FROM `Courses` WHERE `idCourse` = {sqlidCourse}").on(
      'sqlidCourse -> vidCourse
  		).executeUpdate()
    }

	def insert(vCourses: MdlCourses) = DB.withConnection { implicit c =>
  		SQL("INSERT INTO `Courses` (`CourseIDNumber`, `AcademicYear`, `AcademicTerm`, `CourseName`, `CourseDirectorEmail`, `ProgramDirectorEmail`, `CourseDescriptionRedbook`, `CreditHours`, `Prerequisites`, `Corequisites`, `Disqualifiers`, `CourseStrategy`, `CriteriaForPassing`, `AdminInstructions`, `DepartmentID`, `CourseWebsite`, `CourseDescriptionWebsite`) VALUES ({sqlCourseIDNumber}, {sqlAcademicYear}, {sqlAcademicTerm}, {sqlCourseName}, {sqlCourseDirectorEmail}, {sqlProgramDirectorEmail}, {sqlCourseDescriptionRedbook}, {sqlCreditHours}, {sqlPrerequisites}, {sqlCorequisites}, {sqlDisqualifiers}, {sqlCourseStrategy}, {sqlCriteriaForPassing}, {sqlAdminInstructions}, {sqlDepartmentID}, {sqlCourseWebsite}, {sqlCourseDescriptionWebsite})").on('sqlCourseIDNumber -> vCourses.vCourseIDNumber, 'sqlAcademicYear -> vCourses.vAcademicYear, 'sqlAcademicTerm -> vCourses.vAcademicTerm, 'sqlCourseName -> vCourses.vCourseName, 'sqlCourseDirectorEmail -> vCourses.vCourseDirectorEmail, 'sqlProgramDirectorEmail -> vCourses.vProgramDirectorEmail, 'sqlCourseDescriptionRedbook -> vCourses.vCourseDescriptionRedbook, 'sqlCreditHours -> vCourses.vCreditHours, 'sqlPrerequisites -> vCourses.vPrerequisites, 'sqlCorequisites -> vCourses.vCorequisites, 'sqlDisqualifiers -> vCourses.vDisqualifiers, 'sqlCourseStrategy -> vCourses.vCourseStrategy, 'sqlCriteriaForPassing -> vCourses.vCriteriaForPassing, 'sqlAdminInstructions -> vCourses.vAdminInstructions, 'sqlDepartmentID -> vCourses.vDepartmentID, 'sqlCourseWebsite -> vCourses.vCourseWebsite, 'sqlCourseDescriptionWebsite -> vCourses.vCourseDescriptionWebsite).executeInsert()
	}

	def update(vCourses: MdlCourses) = DB.withConnection { implicit c =>
  		SQL("UPDATE `Courses` SET `CourseIDNumber` = {sqlCourseIDNumber}, `AcademicYear` = {sqlAcademicYear}, `AcademicTerm` = {sqlAcademicTerm}, `CourseName` = {sqlCourseName}, `CourseDirectorEmail` = {sqlCourseDirectorEmail}, `ProgramDirectorEmail` = {sqlProgramDirectorEmail}, `CourseDescriptionRedbook` = {sqlCourseDescriptionRedbook}, `CreditHours` = {sqlCreditHours}, `Prerequisites` = {sqlPrerequisites}, `Corequisites` = {sqlCorequisites}, `Disqualifiers` = {sqlDisqualifiers}, `CourseStrategy` = {sqlCourseStrategy}, `CriteriaForPassing` = {sqlCriteriaForPassing}, `AdminInstructions` = {sqlAdminInstructions}, `DepartmentID` = {sqlDepartmentID}, `CourseWebsite` = {sqlCourseWebsite}, `CourseDescriptionWebsite` = {sqlCourseDescriptionWebsite} WHERE `idCourse` = {sqlidCourse}").on('sqlidCourse -> vCourses.vidCourse, 'sqlCourseIDNumber -> vCourses.vCourseIDNumber, 'sqlAcademicYear -> vCourses.vAcademicYear, 'sqlAcademicTerm -> vCourses.vAcademicTerm, 'sqlCourseName -> vCourses.vCourseName, 'sqlCourseDirectorEmail -> vCourses.vCourseDirectorEmail, 'sqlProgramDirectorEmail -> vCourses.vProgramDirectorEmail, 'sqlCourseDescriptionRedbook -> vCourses.vCourseDescriptionRedbook, 'sqlCreditHours -> vCourses.vCreditHours, 'sqlPrerequisites -> vCourses.vPrerequisites, 'sqlCorequisites -> vCourses.vCorequisites, 'sqlDisqualifiers -> vCourses.vDisqualifiers, 'sqlCourseStrategy -> vCourses.vCourseStrategy, 'sqlCriteriaForPassing -> vCourses.vCriteriaForPassing, 'sqlAdminInstructions -> vCourses.vAdminInstructions, 'sqlDepartmentID -> vCourses.vDepartmentID, 'sqlCourseWebsite -> vCourses.vCourseWebsite, 'sqlCourseDescriptionWebsite -> vCourses.vCourseDescriptionWebsite).executeUpdate()

  }

}