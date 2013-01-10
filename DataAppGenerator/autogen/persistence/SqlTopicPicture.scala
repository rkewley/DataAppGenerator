
package persistence

import models.MdlTopicPicture
import anorm._
import anorm.SqlParser._
import play.api.db._
import play.api.Play.current
import play.Logger

object SqlTopicPicture {

  val vTopicPicture = {
    get[Long]("idTopicPicture") ~
	get[Long]("Topic") ~
	get[String]("Hyperlink") ~
	get[String]("AlternateText") ~
	get[String]("Caption") map { case
    vidTopicPicture ~
		vTopic ~
		vHyperlink ~
		vAlternateText ~
		vCaption =>
    MdlTopicPicture(vidTopicPicture,
		vTopic,
		vHyperlink,
		vAlternateText,
		vCaption)
    }
  }

  	def all: List[MdlTopicPicture] = DB.withConnection { implicit c =>
  		SQL("select * from `TopicPicture`").as(vTopicPicture *)
	}

	def select(vidTopicPicture: Long): MdlTopicPicture = DB.withConnection { implicit c =>
  		SQL("select * from `TopicPicture` WHERE `idTopicPicture` = {sqlidTopicPicture}").on(
  			'sqlidTopicPicture -> vidTopicPicture).as(vTopicPicture *).head
	}

  	def selectWhere(where: String): List[MdlTopicPicture] = DB.withConnection { implicit c =>
  		SQL("select * from `TopicPicture` WHERE " + where).as(vTopicPicture *)
	}

	def delete(vidTopicPicture: Long) = DB.withConnection { implicit c =>
  		SQL("DELETE FROM `TopicPicture` WHERE `idTopicPicture` = {sqlidTopicPicture}").on(
      'sqlidTopicPicture -> vidTopicPicture
  		).executeUpdate()
    }

	def insert(vTopicPicture: MdlTopicPicture) = DB.withConnection { implicit c =>
  		SQL("INSERT INTO `TopicPicture` (`Topic`, `Hyperlink`, `AlternateText`, `Caption`) VALUES ({sqlTopic}, {sqlHyperlink}, {sqlAlternateText}, {sqlCaption})").on('sqlTopic -> vTopicPicture.vTopic, 'sqlHyperlink -> vTopicPicture.vHyperlink, 'sqlAlternateText -> vTopicPicture.vAlternateText, 'sqlCaption -> vTopicPicture.vCaption).executeInsert()
	}

	def update(vTopicPicture: MdlTopicPicture) = DB.withConnection { implicit c =>
  		SQL("UPDATE `TopicPicture` SET `Topic` = {sqlTopic}, `Hyperlink` = {sqlHyperlink}, `AlternateText` = {sqlAlternateText}, `Caption` = {sqlCaption} WHERE `idTopicPicture` = {sqlidTopicPicture}").on('sqlidTopicPicture -> vTopicPicture.vidTopicPicture, 'sqlTopic -> vTopicPicture.vTopic, 'sqlHyperlink -> vTopicPicture.vHyperlink, 'sqlAlternateText -> vTopicPicture.vAlternateText, 'sqlCaption -> vTopicPicture.vCaption).executeUpdate()

  }

}