
package persistence

import models.MdlTopics
import anorm._
import anorm.SqlParser._
import play.api.db._
import play.api.Play.current
import play.Logger

object SqlTopics {

  val vTopics = {
    get[Long]("TopicsIndex") ~
	get[String]("Topic") ~
	get[String]("TopicShortDescription") ~
	get[String]("TopicDetailedDescription") map { case
    vTopicsIndex ~
		vTopic ~
		vTopicShortDescription ~
		vTopicDetailedDescription =>
    MdlTopics(vTopicsIndex,
		vTopic,
		vTopicShortDescription,
		vTopicDetailedDescription)
    }
  }

  	def all: List[MdlTopics] = DB.withConnection { implicit c =>
  		SQL("select * from `Topics`").as(vTopics *)
	}

	def select(vTopicsIndex: Long): MdlTopics = DB.withConnection { implicit c =>
  		SQL("select * from `Topics` WHERE `TopicsIndex` = {sqlTopicsIndex}").on(
  			'sqlTopicsIndex -> vTopicsIndex).as(vTopics *).head
	}

  	def selectWhere(where: String): List[MdlTopics] = DB.withConnection { implicit c =>
  		SQL("select * from `Topics` WHERE " + where).as(vTopics *)
	}

	def delete(vTopicsIndex: Long) = DB.withConnection { implicit c =>
  		SQL("DELETE FROM `Topics` WHERE `TopicsIndex` = {sqlTopicsIndex}").on(
      'sqlTopicsIndex -> vTopicsIndex
  		).executeUpdate()
    }

	def insert(vTopics: MdlTopics) = DB.withConnection { implicit c =>
  		SQL("INSERT INTO `Topics` (`Topic`, `TopicShortDescription`, `TopicDetailedDescription`) VALUES ({sqlTopic}, {sqlTopicShortDescription}, {sqlTopicDetailedDescription})").on('sqlTopic -> vTopics.vTopic, 'sqlTopicShortDescription -> vTopics.vTopicShortDescription, 'sqlTopicDetailedDescription -> vTopics.vTopicDetailedDescription).executeInsert()
	}

	def update(vTopics: MdlTopics) = DB.withConnection { implicit c =>
  		SQL("UPDATE `Topics` SET `Topic` = {sqlTopic}, `TopicShortDescription` = {sqlTopicShortDescription}, `TopicDetailedDescription` = {sqlTopicDetailedDescription} WHERE `TopicsIndex` = {sqlTopicsIndex}").on('sqlTopicsIndex -> vTopics.vTopicsIndex, 'sqlTopic -> vTopics.vTopic, 'sqlTopicShortDescription -> vTopics.vTopicShortDescription, 'sqlTopicDetailedDescription -> vTopics.vTopicDetailedDescription).executeUpdate()

  }

}