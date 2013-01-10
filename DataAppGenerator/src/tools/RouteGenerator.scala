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
GET     /%s/:%s						controllers.%s.%s(%s: %s)
GET		/%s/:id					controllers.%s.%s(id: %s)
GET 	/%s/:id					controllers.%s.%s(id: %s)
GET 	/%s/:%s					controllers.%s.%s(%s: %s)
POST	/%s/:id				controllers.%s.%s(id: %s)
POST 	/%s/:newEntry			controllers.%s.%s(newEntry: Int)
""" format (tableName, 
    listName, fixedForeignKeyName, controllerName, listName, fixedForeignKeyName, fixedForeignKeyType,
    	showName, controllerName, showName, primaryKeyType,
    	editName, controllerName, editName, primaryKeyType,
    	createName, fixedForeignKeyName, controllerName, createName, fixedForeignKeyName, fixedForeignKeyType,
    	deleteName, controllerName, deleteName, primaryKeyType,
    	saveName, controllerName, saveName)
    
      case None =>
    """
# %s Routes
GET     /%s						controllers.%s.%s
GET		/%s/:id					controllers.%s.%s(id: %s)
GET 	/%s/:id					controllers.%s.%s(id: %s)
GET 	/%s					controllers.%s.%s
POST	/%s/:id				controllers.%s.%s(id: %s)
POST 	/%s/:newEntry			controllers.%s.%s(newEntry: Int)
""" format (tableName, listName, controllerName, listName,
    	showName, controllerName, showName, primaryKeyType,
    	editName, controllerName, editName, primaryKeyType,
    	createName, controllerName, createName,
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