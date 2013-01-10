package tools


object ModelGenerator {
  
  def defautValuesType(field: FieldMetaData): String = {
    field.clazz match {
      case "java.lang.String" => "\"\""
      case "java.lang.Boolean" => "false"
      case "java.lang.Integer" => "0"
      case "java.math.BigDecimal" => "0.0"
      case "java.lang.Double" => "0.0"
     case other => "0"
    }
  }  

  def formatField(field:FieldMetaData):String = {
    "\t" + DataAppGenerator.valueName(field.column) + " : " + DataAppGenerator.fieldType(field)
  }
    

  def genClassDef(tableName:String, fields:List[FieldMetaData], primaryKeys: List[FieldMetaData]):String = {
    val className = DataAppGenerator.modelName(tableName)

    val fieldList = fields.map(formatField(_)).mkString(",\n")
    val defaultValues = fields.map(defautValuesType(_)).mkString(", ")
    val firstPrimaryKeyValue = DataAppGenerator.valueName(primaryKeys.head.column)
    """    
 package models
    
 case class %s (
 %s
    )  {
    
      def this() = this(%s)

  	  def validate: Boolean = true
    
	  def validationErrors: String = ""
    
      def selectIdentifier: (String, String) = %s.toString -> %s.toString
    
      def compare(a: %s, b: %s) = a.%s.compareTo(b.%s)
}
    """ format (className, fieldList, defaultValues, firstPrimaryKeyValue, firstPrimaryKeyValue,
                className, className, firstPrimaryKeyValue, firstPrimaryKeyValue)
  }
  
  def writeModel(tableName:String, db: DB) {
    val fields = db.getFieldList(tableName)
    val primaryKeys = db.getPrimaryKeys(tableName)
    println("Generating model data for " + tableName + ", number of primary keys is " + primaryKeys.size)
    val modelData = genClassDef(tableName, fields, primaryKeys)
    println(modelData)
    DataAppGenerator.writeStringToFile("autogen/models/" + DataAppGenerator.modelName(tableName) + ".scala", modelData)
  }

}