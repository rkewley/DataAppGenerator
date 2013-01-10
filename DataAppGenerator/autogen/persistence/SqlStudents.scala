
package persistence

import models.MdlStudents
import anorm._
import anorm.SqlParser._
import play.api.db._
import play.api.Play.current
import play.Logger

object SqlStudents {

  val vStudents = {
    get[Long]("idStudent") ~
	get[String]("Email") ~
	get[String]("LastName") ~
	get[String]("FirstName") ~
	get[String]("PreferredFirstName") ~
	get[String]("CadetCompany") ~
	get[String]("Major") ~
	get[String]("BarracksRoom") ~
	get[String]("CellPhone") ~
	get[String]("HomeTown") ~
	get[String]("CadetSports") ~
	get[String]("CadetActivities") ~
	get[String]("PlannedBranch") ~
	get[String]("PlannedPost") ~
	get[String]("ExpectationsFromCourse") ~
	get[String]("TopicsToDiscuss") map { case
    vidStudent ~
		vEmail ~
		vLastName ~
		vFirstName ~
		vPreferredFirstName ~
		vCadetCompany ~
		vMajor ~
		vBarracksRoom ~
		vCellPhone ~
		vHomeTown ~
		vCadetSports ~
		vCadetActivities ~
		vPlannedBranch ~
		vPlannedPost ~
		vExpectationsFromCourse ~
		vTopicsToDiscuss =>
    MdlStudents(vidStudent,
		vEmail,
		vLastName,
		vFirstName,
		vPreferredFirstName,
		vCadetCompany,
		vMajor,
		vBarracksRoom,
		vCellPhone,
		vHomeTown,
		vCadetSports,
		vCadetActivities,
		vPlannedBranch,
		vPlannedPost,
		vExpectationsFromCourse,
		vTopicsToDiscuss)
    }
  }

  	def all: List[MdlStudents] = DB.withConnection { implicit c =>
  		SQL("select * from `Students`").as(vStudents *)
	}

	def select(vidStudent: Long): MdlStudents = DB.withConnection { implicit c =>
  		SQL("select * from `Students` WHERE `idStudent` = {sqlidStudent}").on(
  			'sqlidStudent -> vidStudent).as(vStudents *).head
	}

  	def selectWhere(where: String): List[MdlStudents] = DB.withConnection { implicit c =>
  		SQL("select * from `Students` WHERE " + where).as(vStudents *)
	}

	def delete(vidStudent: Long) = DB.withConnection { implicit c =>
  		SQL("DELETE FROM `Students` WHERE `idStudent` = {sqlidStudent}").on(
      'sqlidStudent -> vidStudent
  		).executeUpdate()
    }

	def insert(vStudents: MdlStudents) = DB.withConnection { implicit c =>
  		SQL("INSERT INTO `Students` (`Email`, `LastName`, `FirstName`, `PreferredFirstName`, `CadetCompany`, `Major`, `BarracksRoom`, `CellPhone`, `HomeTown`, `CadetSports`, `CadetActivities`, `PlannedBranch`, `PlannedPost`, `ExpectationsFromCourse`, `TopicsToDiscuss`) VALUES ({sqlEmail}, {sqlLastName}, {sqlFirstName}, {sqlPreferredFirstName}, {sqlCadetCompany}, {sqlMajor}, {sqlBarracksRoom}, {sqlCellPhone}, {sqlHomeTown}, {sqlCadetSports}, {sqlCadetActivities}, {sqlPlannedBranch}, {sqlPlannedPost}, {sqlExpectationsFromCourse}, {sqlTopicsToDiscuss})").on('sqlEmail -> vStudents.vEmail, 'sqlLastName -> vStudents.vLastName, 'sqlFirstName -> vStudents.vFirstName, 'sqlPreferredFirstName -> vStudents.vPreferredFirstName, 'sqlCadetCompany -> vStudents.vCadetCompany, 'sqlMajor -> vStudents.vMajor, 'sqlBarracksRoom -> vStudents.vBarracksRoom, 'sqlCellPhone -> vStudents.vCellPhone, 'sqlHomeTown -> vStudents.vHomeTown, 'sqlCadetSports -> vStudents.vCadetSports, 'sqlCadetActivities -> vStudents.vCadetActivities, 'sqlPlannedBranch -> vStudents.vPlannedBranch, 'sqlPlannedPost -> vStudents.vPlannedPost, 'sqlExpectationsFromCourse -> vStudents.vExpectationsFromCourse, 'sqlTopicsToDiscuss -> vStudents.vTopicsToDiscuss).executeInsert()
	}

	def update(vStudents: MdlStudents) = DB.withConnection { implicit c =>
  		SQL("UPDATE `Students` SET `Email` = {sqlEmail}, `LastName` = {sqlLastName}, `FirstName` = {sqlFirstName}, `PreferredFirstName` = {sqlPreferredFirstName}, `CadetCompany` = {sqlCadetCompany}, `Major` = {sqlMajor}, `BarracksRoom` = {sqlBarracksRoom}, `CellPhone` = {sqlCellPhone}, `HomeTown` = {sqlHomeTown}, `CadetSports` = {sqlCadetSports}, `CadetActivities` = {sqlCadetActivities}, `PlannedBranch` = {sqlPlannedBranch}, `PlannedPost` = {sqlPlannedPost}, `ExpectationsFromCourse` = {sqlExpectationsFromCourse}, `TopicsToDiscuss` = {sqlTopicsToDiscuss} WHERE `idStudent` = {sqlidStudent}").on('sqlidStudent -> vStudents.vidStudent, 'sqlEmail -> vStudents.vEmail, 'sqlLastName -> vStudents.vLastName, 'sqlFirstName -> vStudents.vFirstName, 'sqlPreferredFirstName -> vStudents.vPreferredFirstName, 'sqlCadetCompany -> vStudents.vCadetCompany, 'sqlMajor -> vStudents.vMajor, 'sqlBarracksRoom -> vStudents.vBarracksRoom, 'sqlCellPhone -> vStudents.vCellPhone, 'sqlHomeTown -> vStudents.vHomeTown, 'sqlCadetSports -> vStudents.vCadetSports, 'sqlCadetActivities -> vStudents.vCadetActivities, 'sqlPlannedBranch -> vStudents.vPlannedBranch, 'sqlPlannedPost -> vStudents.vPlannedPost, 'sqlExpectationsFromCourse -> vStudents.vExpectationsFromCourse, 'sqlTopicsToDiscuss -> vStudents.vTopicsToDiscuss).executeUpdate()

  }

}