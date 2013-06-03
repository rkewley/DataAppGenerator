package tools

object ShowGenerator {
  
  def genHeader(tableName: String): String = {
    """
@(%s: %s)

    @import slick.AppDB
		  
    %s
    
     <div class="column span-16">
		  <h3>%s Information</h3>
		  <dl>
""" format(DataAppGenerator.valueName(tableName), DataAppGenerator.modelName(tableName), 
    DataAppGenerator.setInSiteContext("Systems Body of Knowledge", ""),
    tableName)
  }
  
  def genFkShowDiv(tableName: String, foreignKey: ForeignKeyMetaData): String = {
    val fk = foreignKey.fk
    val pk = foreignKey.pk
"""
    		@defining(%s.select(%s.%s).get){pkModel =>
      		  <dt>%s</dt>
      		  <dd><a href="@routes.%s.show(%s.%s)">@pkModel.selectIdentifier._2</a></dd>
    		}
""" format(DataAppGenerator.slickDBName(pk.table), DataAppGenerator.valueName(fk.table), DataAppGenerator.valueName(fk.column),
			foreignKey.fk.column, 
			DataAppGenerator.controllerName(pk.table), DataAppGenerator.valueName(fk.table), DataAppGenerator.valueName(fk.column))
  }
    
  
  def genNormalShowDiv(tableName: String, field: FieldMetaData): String = {"""
		  	<dt>%s</dt>
		  	<dd>@Html(%s.%s.toString)</dd>
""" format(field.column, DataAppGenerator.valueName(tableName), DataAppGenerator.valueName(field.column))
  }
  
  def genShowInformation(tableName: String, fields: List[FieldMetaData], db: DB, fixedForeignKey: Option[ForeignKeyMetaData]): String = {
    val foreignKeys = db.getForeignKeys(tableName)
    val routeListFkIndex = fixedForeignKey match {
      case Some(foreignKey) => """(%s.%s)""" format(DataAppGenerator.valueName(tableName), DataAppGenerator.valueName(foreignKey.fk.column))
      case None => "(0)"
    }
    fields.map(field => db.isKey(field, foreignKeys.map(_.fk)) match {
      case true =>
        genFkShowDiv(tableName, foreignKeys.find(foreignKey => foreignKey.fk.column==field.column).get)
      case false => 
        genNormalShowDiv(tableName, field)
    }).mkString("") + 
"	       </dl>\n" +
"     <div>\n" + 
"       <a href=\"@routes." + DataAppGenerator.controllerName(tableName) + ".list" + routeListFkIndex + "\">Return to listing</a>\n" +
"     </div>\n" +
"	</div>\n" +
"   <div class=\"column span-4 last\">\n" +
"   </div>\n" +
"  </div>\n" + 
"  }\n"
  }
    
  def writeShowView(tableName: String, db: DB, fixedForeignKey: Option[ForeignKeyMetaData]) {
    val fields = db.getFieldList(tableName)
    val showData = genHeader(tableName) + genShowInformation(tableName, fields, db, fixedForeignKey)
    println(showData)
    DataAppGenerator.writeStringToFile("autogen/views/viewshow/" + DataAppGenerator.showName(tableName) + ".scala.html", showData)
  }
  

}