
package persistence

import models.MdlUser
import anorm._
import anorm.SqlParser._
import play.api.db._
import play.api.Play.current
import play.Logger

object SqlUser {

  val vUser = {
    get[String]("email") ~
	get[String]("password") ~
	get[String]("permissions") map { case
    vemail ~
		vpassword ~
		vpermissions =>
    MdlUser(vemail,
		vpassword,
		vpermissions)
    }
  }

  	def all: List[MdlUser] = DB.withConnection { implicit c =>
  		SQL("select * from `User`").as(vUser *)
	}

	def select(vemail: String): MdlUser = DB.withConnection { implicit c =>
  		SQL("select * from `User` WHERE `email` = {sqlemail}").on(
  			'sqlemail -> vemail).as(vUser *).head
	}

  	def selectWhere(where: String): List[MdlUser] = DB.withConnection { implicit c =>
  		SQL("select * from `User` WHERE " + where).as(vUser *)
	}

	def delete(vemail: String) = DB.withConnection { implicit c =>
  		SQL("DELETE FROM `User` WHERE `email` = {sqlemail}").on(
      'sqlemail -> vemail
  		).executeUpdate()
    }

	def insert(vUser: MdlUser) = DB.withConnection { implicit c =>
  		SQL("INSERT INTO `User` (`email`, `password`, `permissions`) VALUES ({sqlemail}, {sqlpassword}, {sqlpermissions})").on('sqlemail -> vUser.vemail, 'sqlpassword -> vUser.vpassword, 'sqlpermissions -> vUser.vpermissions).executeInsert()
	}

	def update(vUser: MdlUser) = DB.withConnection { implicit c =>
  		SQL("UPDATE `User` SET `password` = {sqlpassword}, `permissions` = {sqlpermissions} WHERE `email` = {sqlemail}").on('sqlemail -> vUser.vemail, 'sqlpassword -> vUser.vpassword, 'sqlpermissions -> vUser.vpermissions).executeUpdate()

  }

}