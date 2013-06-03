package tools

object FormGenerator {
  
  def genHeader(tableName: String): String = {
    val modelName = DataAppGenerator.modelName(tableName)
    val controllerName = DataAppGenerator.controllerName(tableName)
    val saveName = DataAppGenerator.saveName(tableName)
    """
@(f: Form[%s], newEntry: Int)

@import helper._
@import persistence._

%s

<div class="column span-20 last">
    <fieldset>
    @helper.form(routes.%s.save(newEntry)) {

""" format(modelName, 
    DataAppGenerator.setInSiteContext("Systems Body of Knowledge", ""),
    controllerName)
  }
  
  def genHeaderFk(tableName: String, foreignKey: ForeignKeyMetaData): String = {
    val modelName = DataAppGenerator.modelName(tableName)
    val controllerName = DataAppGenerator.controllerName(tableName)
    val saveName = DataAppGenerator.saveName(tableName)
    """
@(f: Form[%s], newEntry: Int)

@import helper._
@import persistence._

%s

@defining(f("%s").value.get.toLong) { %s =>  
<div class="column span-20 last">
    <fieldset>
    @helper.form(routes.%s.%s(newEntry)) {

""" format(modelName, 
    DataAppGenerator.setInSiteContext("Systems Body of Knowledge", ""),
    DataAppGenerator.formValue(foreignKey.fk.column), DataAppGenerator.valueName(foreignKey.fk.column),
    controllerName, saveName)
  }
  def genHiddenInput(field: FieldMetaData): String = {
    val formValue = DataAppGenerator.formValue(field.column)
    """
            @defining(f("%s")) { %s =>
                <input type="hidden" name="@%s.name" id="@%s.id" value="@%s.value">
			}
    """ format(formValue, formValue, formValue, formValue, formValue)
  }
    
  def genInput(field: FieldMetaData): String = {
    val formValue = DataAppGenerator.formValue(field.column)
    field.clazz match {
      case "java.lang.String" =>
      	"""
            @textarea(field = f("%s"), args = 'rows -> 2, 'cols -> 100, '_label-> "%s")
        	@*@inputText(f("%s"), '_label -> "%s")*@
""" format (formValue, field.column, formValue, field.column)
      case "java.lang.Double" =>
         """
            @inputText(f("%s"), '_label -> "%s")
""" format (formValue, field.column)
      case "java.lang.Integer" =>
         """
            @inputText(f("%s"), '_label -> "%s")
""" format (formValue, field.column)
      case "java.lang.Long" =>
         """
            @inputText(f("%s"), '_label -> "%s")
""" format (formValue, field.column)
      case "java.lang.Boolean" =>
         """
            @helper.checkbox(f("%s"), '_label -> "%s", '_help->"")
""" format (formValue, field.column)
      case(str) => throw new Exception("Can't create form helper for clazz: " + str)
    }
  }
    
  
  def genKeys(tableName: String, primaryKeys: List[FieldMetaData]): String = {
    val hiddenEntries = primaryKeys.map(genHiddenInput).mkString("\n")
    val pkFieldsToInput = primaryKeys.filter(pk => !pk.isAutoIncrement)
    val inputHelpers = primaryKeys.map(pk => pk.isAutoIncrement match {
      case true => genHiddenInput(pk)
      case false => genInput(pk)
      }).mkString("\n")
    """
        @if(newEntry == 0) {
		  %s
		}
		@if(newEntry != 0) {
     		%s
     	}
""" format(hiddenEntries, inputHelpers)
 
  }
  
  def genForeignKeyInput(foreignKeyMetaData: ForeignKeyMetaData) = {
    val pkTable = foreignKeyMetaData.pk.table
    val fkColumn = foreignKeyMetaData.fk.column
    val tableValue = DataAppGenerator.valueName(pkTable)
    """
            @{val select%s = %s.all.map(%s => %s.selectIdentifier).toSeq
            select(field = f("%s"), options = select%s, args = '_label-> "%s")}
    		<a href="@routes.%s.%s">Create new %s</a>
""" format(tableValue, DataAppGenerator.sqlObjectName(pkTable), tableValue, tableValue, 
			DataAppGenerator.formValue(fkColumn), tableValue, fkColumn,
			DataAppGenerator.controllerName(pkTable), DataAppGenerator.createName(pkTable), pkTable)
  }
    
  def genInputFields(fields: List[FieldMetaData], primaryKeys: List[FieldMetaData], foreignKeys: List[ForeignKeyMetaData], db: DB, fixedForeignKey: Option[ForeignKeyMetaData]): String = {
    fields.map(field => {
      if (db.isKey(field, primaryKeys)) {
        ""
      }
      else if (db.isKey(field, foreignKeys.map(foreignKey => foreignKey.fk))) {
        if (fixedForeignKey.isDefined && field.column == fixedForeignKey.get.fk.column) {
          genHiddenInput(field)
        } else {
          genForeignKeyInput(db.getKeyMetaData(field, foreignKeys))
        }
      }
      else {
        genInput(field)
      }
    }).mkString("")
  }
  
  def genSubmit(tableName: String): String = {
	"""
       		<fieldset style="margin-top: 10px;">
          		<input type="submit" value="Submit" />
          		<a href="@routes.%s.%s(0)">Cancel and return to listing</a>
      		</fieldset>
        }
	  	</fieldset></div>
    </div>
  </div>
  }
""" format (DataAppGenerator.controllerName(tableName), DataAppGenerator.listName(tableName))

  }
  
  def genSubmitFk(tableName: String, fixedForeignKey: ForeignKeyMetaData): String = {
	"""
       		<fieldset style="margin-top: 10px;">
          		<input type="submit" value="Submit" />
          		<a href="@routes.%s.list(%s)">Cancel and return to listing</a>
      		</fieldset>
        }
	  	</fieldset></div>
    </div>
  </div>
  }
  }
""" format (DataAppGenerator.controllerName(tableName), DataAppGenerator.valueName(fixedForeignKey.fk.column))

  }
  
  def writeForm(tableName:String, db: DB, fixedForeignKey: Option[ForeignKeyMetaData]) {
    val fields = db.getFieldList(tableName)
    val primaryKeys = db.getPrimaryKeys(tableName)
    val foreignKeys = db.getForeignKeys(tableName)
    val formString = fixedForeignKey match {
      case Some(fk) => genHeaderFk(tableName, fk) + genKeys(tableName, primaryKeys) + 
    	genInputFields(fields, primaryKeys, foreignKeys, db, fixedForeignKey) + genSubmitFk(tableName, fk)
      case None => genHeader(tableName) + genKeys(tableName, primaryKeys) + 
    	genInputFields(fields, primaryKeys, foreignKeys, db, fixedForeignKey) + genSubmit(tableName)
    }
    println(formString)
    DataAppGenerator.writeStringToFile("autogen/views/viewforms/" + DataAppGenerator.formName(tableName) + ".scala.html", formString)

  }


}