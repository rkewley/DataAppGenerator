package tools

object RouteGenerator {
  
  def genRoutes(tableName: String, db: DB, fixedForeignKey: Option[ForeignKeyMetaData]): String = {
    val controllerName = DataAppGenerator.controllerName(tableName)
    val listName = DataAppGenerator.listName(tableName)
    val showName = DataAppGenerator.showName(tableName)
    val editName = DataAppGenerator.editName(tableName)
    val createName = DataAppGenerator.createName(tableName)
    val deleteName = DataAppGenerator.deleteName(tableName)
    val saveName = DataAppGenerator.saveName(tableName)
    
    val primaryKeyType = DataAppGenerator.fieldType(db.getPrimaryKeys(tableName).head)
    val tn = tableName
    
    fixedForeignKey match {
      case Some(foreignKey) => 
        val fixedForeignKeyName = "id" + DataAppGenerator.noWhitespace(foreignKey.pk.table)
        val fixedForeignKeyType = DataAppGenerator.fieldType(foreignKey.fk)
    """
# %s Routes
GET     /%s/:%s						controllers.%s.list(%s: %s)
GET		/%s/:id					controllers.%s.show(id: %s)
GET 	/%s/:id					controllers.%s.edit(id: %s)
GET 	/%s/:%s					controllers.%s.create(%s: %s)
POST	/%s/:id				controllers.%s.delete(id: %s)
POST 	/%s/:newEntry			controllers.%s.save(newEntry: Int)
""" format (tableName, 
    listName, fixedForeignKeyName, controllerName, fixedForeignKeyName, fixedForeignKeyType,
    	showName, controllerName, primaryKeyType,
    	editName, controllerName, primaryKeyType,
    	createName, fixedForeignKeyName, controllerName, fixedForeignKeyName, fixedForeignKeyType,
    	deleteName, controllerName, primaryKeyType,
    	saveName, controllerName)
    
      case None =>
    """
# %s Routes
GET     /%s/:id						controllers.%s.%s(id: %s)
GET		/%s/:id					controllers.%s.%s(id: %s)
GET 	/%s/:id					controllers.%s.%s(id: %s)
GET 	/%s/:id					controllers.%s.%s(id: %s)
POST	/%s/:id				controllers.%s.%s(id: %s)
POST 	/%s/:newEntry			controllers.%s.%s(newEntry: Int)
""" format (tableName, 
        listName, controllerName, listName, Long,
    	showName, controllerName, showName, primaryKeyType,
    	editName, controllerName, editName, primaryKeyType,
    	createName, controllerName, createName, Long,
    	deleteName, controllerName, deleteName, primaryKeyType,
    	saveName, controllerName, saveName)
    }
  }
  
  def writeRoutes(tableName:String, db: DB, fixedForeignKey: Option[ForeignKeyMetaData]) {
    val routeData = genRoutes(tableName, db, fixedForeignKey)
    println(routeData)
    DataAppGenerator.writeStringToFile("autogen/routes/" + DataAppGenerator.noWhitespace(tableName) + ".conf", routeData)
  }


}