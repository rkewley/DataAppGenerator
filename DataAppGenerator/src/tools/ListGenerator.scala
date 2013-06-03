package tools

object ListGenerator {
  def genHeader(tableName: String): String = {
    """
@(%s: List[%s])
    
    @import slick.AppDB
    
    %s
    
    <div class="column span-20 last">
		  <h3>%s Listing</h3>
		  <table>
""" format (DataAppGenerator.listName(tableName), DataAppGenerator.modelName(tableName), 
			DataAppGenerator.setInSiteContext("Systems Body of Knowledge", ""),
			    tableName)
  }

  def genHeaderFk(tableName: String, foreignKey: ForeignKeyMetaData): String = {
    """
@(%s: List[%s], id%s: Long)
    
    @import slick.AppDB
    
    %s
    
    <div class="column span-20 last">
		  <h3>%s Listing</h3>
		  <table>
""" format (DataAppGenerator.listName(tableName), DataAppGenerator.modelName(tableName), DataAppGenerator.noWhitespace(foreignKey.pk.table),
			DataAppGenerator.setInSiteContext("Systems Body of Knowledge", ""),
			tableName)
  }

  
  def genHeadings(tableName: String, db: DB): String = {
    def genHeadingItem(field: FieldMetaData): String = "\t\t\t\t<th>" + field.column + "</th>\n" 
    "\t\t\t<tr>\n" + 
    db.getFieldList(tableName).map(field => {
      if(!field.sqlType.endsWith("TEXT")) genHeadingItem(field)
    }).mkString("") + "\t\t\t\t<th>Show</th>\n" + "\t\t\t\t<th>Edit</th>\n" + "\t\t\t\t<th>Delete</th>\n" + 
    "\t\t\t</tr>\n"
  }
  
  def genRows(tableName: String, db: DB): String = {
    val controllerName = DataAppGenerator.controllerName(tableName)
    val editName = DataAppGenerator.editName(tableName)
    val tableValue = DataAppGenerator.valueName(tableName)  
    val deleteName = DataAppGenerator.deleteName(tableName) 
    val foreignKeys = db.getForeignKeys(tableName)
    val firstPrimaryKey = db.getPrimaryKeys(tableName).head.column
    val keyValue = DataAppGenerator.valueName(firstPrimaryKey)
    def genDataItem(field: FieldMetaData): String = {
      db.isKey(field, foreignKeys.map(_.fk)) match {
        case true => genFkDataItem(foreignKeys.find(foreignKey => foreignKey.fk.column == field.column).get)
        case false => genNormalDataItem(field)
      }
    }
    def genFkDataItem(foreignKey: ForeignKeyMetaData): String = {
          val fk = foreignKey.fk
          val pk = foreignKey.pk
"""
    		@defining(%s.select(%s.%s).get){pkModel =>
      		  <td><a href="@routes.%s.show(%s.%s)">@pkModel.selectIdentifier._2</a></td>
    		}
""" format(DataAppGenerator.slickDBName(pk.table), DataAppGenerator.valueName(fk.table), DataAppGenerator.valueName(fk.column),
			DataAppGenerator.controllerName(pk.table), DataAppGenerator.valueName(fk.table), DataAppGenerator.valueName(fk.column))
    }
    def genNormalDataItem(field: FieldMetaData): String = "\t\t\t\t<td>@" + 
    	DataAppGenerator.valueName(tableName)+ "." + DataAppGenerator.valueName(field.column) + "</td>\n"
    def genRowDataItems = {
      db.getFieldList(tableName).map(field => {
        if(!field.sqlType.endsWith("TEXT")) genDataItem(field)
      }).mkString("")
    }
    def genRowItem: String = {"""
    		<tr>
%s
    			<td><a href="@routes.%s.show(%s.%s.get)">Show</a></td>
    			<td><a href="@routes.%s.edit(%s.%s.get)">Edit</a></td>
    	  		<td>
    	  			<form action="@routes.%s.delete(%s.%s)" method="POST">
    	  			<input type="button" onclick="if (confirm('Are you sure you want to delete?\n  This action cannot be undone.')) submit();" value="delete">
    	  			</form>
    	  		</td>
    		</tr>
    
""" format(genRowDataItems, controllerName, tableValue, keyValue,
		controllerName, tableValue, keyValue,
		controllerName, tableValue, keyValue)
    }
    """
            @%s.map { %s =>
    			%s
      		}
""" format(DataAppGenerator.listName(tableName), DataAppGenerator.valueName(tableName), genRowItem)
  }
  
  def genFooter(tableName: String): String = {"""
      	</table>
      <a href="@routes.%s.%s(0)">Create new %s</a>
    </div>
  </div>
  }
""" format (DataAppGenerator.controllerName(tableName), DataAppGenerator.createName(tableName), tableName)
    
  }
  def genFooterFk(tableName: String, foreignKey: ForeignKeyMetaData): String = {"""
      	</table>
      <a href="@routes.%s.create(id%s)">Create new %s</a>
    </div>
  </div>
  }
""" format (DataAppGenerator.controllerName(tableName), DataAppGenerator.noWhitespace(foreignKey.pk.table), tableName)
    
  }
    
  
  def writeListForm(tableName: String, db: DB, foreignKey: Option[ForeignKeyMetaData]) {
    val listData = foreignKey match {
      case Some(fk) => genHeaderFk(tableName, fk) + genHeadings(tableName, db) + genRows(tableName, db) + genFooterFk(tableName, fk)
      case (_) => genHeader(tableName) + genHeadings(tableName, db) + genRows(tableName, db) + genFooter(tableName)
    }
    println(listData)
    DataAppGenerator.writeStringToFile("autogen/views/viewlist/" + DataAppGenerator.listName(tableName) + ".scala.html", listData)
  }

}