
package persistence

import models.MdlDepartments
import anorm._
import anorm.SqlParser._
import play.api.db._
import play.api.Play.current
import play.Logger

object SqlDepartments {

  val vDepartments = {
    get[Long]("DepartmentID") ~
	get[String]("DepartmentName") ~
	get[String]("DepartmentHead") ~
	get[String]("DepartmentHopePage") map { case
    vDepartmentID ~
		vDepartmentName ~
		vDepartmentHead ~
		vDepartmentHopePage =>
    MdlDepartments(vDepartmentID,
		vDepartmentName,
		vDepartmentHead,
		vDepartmentHopePage)
    }
  }

  	def all: List[MdlDepartments] = DB.withConnection { implicit c =>
  		SQL("select * from `Departments`").as(vDepartments *)
	}

	def select(vDepartmentID: Long): MdlDepartments = DB.withConnection { implicit c =>
  		SQL("select * from `Departments` WHERE `DepartmentID` = {sqlDepartmentID}").on(
  			'sqlDepartmentID -> vDepartmentID).as(vDepartments *).head
	}

  	def selectWhere(where: String): List[MdlDepartments] = DB.withConnection { implicit c =>
  		SQL("select * from `Departments` WHERE " + where).as(vDepartments *)
	}

	def delete(vDepartmentID: Long) = DB.withConnection { implicit c =>
  		SQL("DELETE FROM `Departments` WHERE `DepartmentID` = {sqlDepartmentID}").on(
      'sqlDepartmentID -> vDepartmentID
  		).executeUpdate()
    }

	def insert(vDepartments: MdlDepartments) = DB.withConnection { implicit c =>
  		SQL("INSERT INTO `Departments` (`DepartmentName`, `DepartmentHead`, `DepartmentHopePage`) VALUES ({sqlDepartmentName}, {sqlDepartmentHead}, {sqlDepartmentHopePage})").on('sqlDepartmentName -> vDepartments.vDepartmentName, 'sqlDepartmentHead -> vDepartments.vDepartmentHead, 'sqlDepartmentHopePage -> vDepartments.vDepartmentHopePage).executeInsert()
	}

	def update(vDepartments: MdlDepartments) = DB.withConnection { implicit c =>
  		SQL("UPDATE `Departments` SET `DepartmentName` = {sqlDepartmentName}, `DepartmentHead` = {sqlDepartmentHead}, `DepartmentHopePage` = {sqlDepartmentHopePage} WHERE `DepartmentID` = {sqlDepartmentID}").on('sqlDepartmentID -> vDepartments.vDepartmentID, 'sqlDepartmentName -> vDepartments.vDepartmentName, 'sqlDepartmentHead -> vDepartments.vDepartmentHead, 'sqlDepartmentHopePage -> vDepartments.vDepartmentHopePage).executeUpdate()

  }

}