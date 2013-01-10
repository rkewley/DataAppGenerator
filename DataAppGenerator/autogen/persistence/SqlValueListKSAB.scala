
package persistence

import models.MdlValueListKSAB
import anorm._
import anorm.SqlParser._
import play.api.db._
import play.api.Play.current
import play.Logger

object SqlValueListKSAB {

  val vValueListKSAB = {
    get[String]("KSAB") map { case
    vKSAB =>
    MdlValueListKSAB(vKSAB)
    }
  }

  	def all: List[MdlValueListKSAB] = DB.withConnection { implicit c =>
  		SQL("select * from `ValueListKSAB`").as(vValueListKSAB *)
	}

	def select(vKSAB: String): MdlValueListKSAB = DB.withConnection { implicit c =>
  		SQL("select * from `ValueListKSAB` WHERE `KSAB` = {sqlKSAB}").on(
  			'sqlKSAB -> vKSAB).as(vValueListKSAB *).head
	}

  	def selectWhere(where: String): List[MdlValueListKSAB] = DB.withConnection { implicit c =>
  		SQL("select * from `ValueListKSAB` WHERE " + where).as(vValueListKSAB *)
	}

	def delete(vKSAB: String) = DB.withConnection { implicit c =>
  		SQL("DELETE FROM `ValueListKSAB` WHERE `KSAB` = {sqlKSAB}").on(
      'sqlKSAB -> vKSAB
  		).executeUpdate()
    }

	def insert(vValueListKSAB: MdlValueListKSAB) = DB.withConnection { implicit c =>
  		SQL("INSERT INTO `ValueListKSAB` (`KSAB`) VALUES ({sqlKSAB})").on('sqlKSAB -> vValueListKSAB.vKSAB).executeInsert()
	}

	def update(vValueListKSAB: MdlValueListKSAB) = DB.withConnection { implicit c =>
  		SQL("UPDATE `ValueListKSAB` SET  WHERE `KSAB` = {sqlKSAB}").on('sqlKSAB -> vValueListKSAB.vKSAB).executeUpdate()

  }

}