
package persistence

import models.MdlTerminalLearningObjective
import anorm._
import anorm.SqlParser._
import play.api.db._
import play.api.Play.current
import play.Logger

object SqlTerminalLearningObjective {

  val vTerminalLearningObjective = {
    get[Long]("idTerminalLearningObjective") ~
	get[String]("TerminalLearningObjective") ~
	get[Long]("Topic") ~
	get[String]("Program") map { case
    vidTerminalLearningObjective ~
		vTerminalLearningObjective ~
		vTopic ~
		vProgram =>
    MdlTerminalLearningObjective(vidTerminalLearningObjective,
		vTerminalLearningObjective,
		vTopic,
		vProgram)
    }
  }

  	def all: List[MdlTerminalLearningObjective] = DB.withConnection { implicit c =>
  		SQL("select * from `TerminalLearningObjective`").as(vTerminalLearningObjective *)
	}

	def select(vidTerminalLearningObjective: Long): MdlTerminalLearningObjective = DB.withConnection { implicit c =>
  		SQL("select * from `TerminalLearningObjective` WHERE `idTerminalLearningObjective` = {sqlidTerminalLearningObjective}").on(
  			'sqlidTerminalLearningObjective -> vidTerminalLearningObjective).as(vTerminalLearningObjective *).head
	}

  	def selectWhere(where: String): List[MdlTerminalLearningObjective] = DB.withConnection { implicit c =>
  		SQL("select * from `TerminalLearningObjective` WHERE " + where).as(vTerminalLearningObjective *)
	}

	def delete(vidTerminalLearningObjective: Long) = DB.withConnection { implicit c =>
  		SQL("DELETE FROM `TerminalLearningObjective` WHERE `idTerminalLearningObjective` = {sqlidTerminalLearningObjective}").on(
      'sqlidTerminalLearningObjective -> vidTerminalLearningObjective
  		).executeUpdate()
    }

	def insert(vTerminalLearningObjective: MdlTerminalLearningObjective) = DB.withConnection { implicit c =>
  		SQL("INSERT INTO `TerminalLearningObjective` (`TerminalLearningObjective`, `Topic`, `Program`) VALUES ({sqlTerminalLearningObjective}, {sqlTopic}, {sqlProgram})").on('sqlTerminalLearningObjective -> vTerminalLearningObjective.vTerminalLearningObjective, 'sqlTopic -> vTerminalLearningObjective.vTopic, 'sqlProgram -> vTerminalLearningObjective.vProgram).executeInsert()
	}

	def update(vTerminalLearningObjective: MdlTerminalLearningObjective) = DB.withConnection { implicit c =>
  		SQL("UPDATE `TerminalLearningObjective` SET `TerminalLearningObjective` = {sqlTerminalLearningObjective}, `Topic` = {sqlTopic}, `Program` = {sqlProgram} WHERE `idTerminalLearningObjective` = {sqlidTerminalLearningObjective}").on('sqlidTerminalLearningObjective -> vTerminalLearningObjective.vidTerminalLearningObjective, 'sqlTerminalLearningObjective -> vTerminalLearningObjective.vTerminalLearningObjective, 'sqlTopic -> vTerminalLearningObjective.vTopic, 'sqlProgram -> vTerminalLearningObjective.vProgram).executeUpdate()

  }

}