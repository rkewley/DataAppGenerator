
package persistence

import models.MdlPrograms
import anorm._
import anorm.SqlParser._
import play.api.db._
import play.api.Play.current
import play.Logger

object SqlPrograms {

  val vPrograms = {
    get[String]("Program") ~
	get[String]("Name") ~
	get[String]("Slogan") ~
	get[String]("Information") ~
	get[String]("ProgramDirector") ~
	get[Long]("Department") map { case
    vProgram ~
		vName ~
		vSlogan ~
		vInformation ~
		vProgramDirector ~
		vDepartment =>
    MdlPrograms(vProgram,
		vName,
		vSlogan,
		vInformation,
		vProgramDirector,
		vDepartment)
    }
  }

  	def all: List[MdlPrograms] = DB.withConnection { implicit c =>
  		SQL("select * from `Programs`").as(vPrograms *)
	}

	def select(vProgram: String): MdlPrograms = DB.withConnection { implicit c =>
  		SQL("select * from `Programs` WHERE `Program` = {sqlProgram}").on(
  			'sqlProgram -> vProgram).as(vPrograms *).head
	}

  	def selectWhere(where: String): List[MdlPrograms] = DB.withConnection { implicit c =>
  		SQL("select * from `Programs` WHERE " + where).as(vPrograms *)
	}

	def delete(vProgram: String) = DB.withConnection { implicit c =>
  		SQL("DELETE FROM `Programs` WHERE `Program` = {sqlProgram}").on(
      'sqlProgram -> vProgram
  		).executeUpdate()
    }

	def insert(vPrograms: MdlPrograms) = DB.withConnection { implicit c =>
  		SQL("INSERT INTO `Programs` (`Program`, `Name`, `Slogan`, `Information`, `ProgramDirector`, `Department`) VALUES ({sqlProgram}, {sqlName}, {sqlSlogan}, {sqlInformation}, {sqlProgramDirector}, {sqlDepartment})").on('sqlProgram -> vPrograms.vProgram, 'sqlName -> vPrograms.vName, 'sqlSlogan -> vPrograms.vSlogan, 'sqlInformation -> vPrograms.vInformation, 'sqlProgramDirector -> vPrograms.vProgramDirector, 'sqlDepartment -> vPrograms.vDepartment).executeInsert()
	}

	def update(vPrograms: MdlPrograms) = DB.withConnection { implicit c =>
  		SQL("UPDATE `Programs` SET `Name` = {sqlName}, `Slogan` = {sqlSlogan}, `Information` = {sqlInformation}, `ProgramDirector` = {sqlProgramDirector}, `Department` = {sqlDepartment} WHERE `Program` = {sqlProgram}").on('sqlProgram -> vPrograms.vProgram, 'sqlName -> vPrograms.vName, 'sqlSlogan -> vPrograms.vSlogan, 'sqlInformation -> vPrograms.vInformation, 'sqlProgramDirector -> vPrograms.vProgramDirector, 'sqlDepartment -> vPrograms.vDepartment).executeUpdate()

  }

}