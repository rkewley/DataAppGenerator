package tools


object ModelGenerator {
  
  def defautValuesType(field: FieldMetaData): String = {
    field.clazz match {
      case "java.lang.String" => "\"\""
      case "java.lang.Boolean" => "false"
      case "java.lang.Integer" => "0"
      case "java.math.BigDecimal" => "0.0"
      case "java.lang.Double" => "0.0"
      case "java.sql.Timestamp" => "new Timestamp(0)"
      case "java.sql.Date" => "new Date(0)"
     case other => "0"
    }
  }  

  def formatField(field:FieldMetaData):String = {
    "\t" + DataAppGenerator.valueName(field.column) + " : " + DataAppGenerator.fieldType(field)
  }
 
  def formatPkField(field:FieldMetaData):String = {
    "\t" + DataAppGenerator.valueName(field.column) + " : Option[" + DataAppGenerator.fieldType(field) + "]"
  }


  def genClassDef(tableName:String, fields:List[FieldMetaData], primaryKeys: List[FieldMetaData]):String = {
    val className = DataAppGenerator.modelName(tableName)
    val firstPrimaryKeyValue = DataAppGenerator.valueName(primaryKeys.head.column)
    val firstPrimaryKeyName = DataAppGenerator.noWhitespace(primaryKeys.head.column)
    println("First primary key value is: " + firstPrimaryKeyValue)
    val fieldList = fields.map(field => 
      (firstPrimaryKeyName == field.column) match {
        case true =>
          println("Formatting primary key: " + field.column)
          formatPkField(field)
        case false =>
          formatField(field)
      }).mkString(",\n")
    val defaultValues = fields.map(field =>
      (firstPrimaryKeyName == field.column) match {
        case true =>
          "Option(" + defautValuesType(field) + ")"
        case false =>
          defautValuesType(field)
      }).mkString(", ")
      
    
    //val firstPrimaryKeyValue = DataAppGenerator.valueName(primaryKeys.head.column)
    primaryKeys.isEmpty match {
      case true => 
    """    
 package models
    
 case class %s (
 %s
    ) extends Mdl[%s] {
    
      def this() = this(%s)

  	  def validate: Boolean = true
    
	  def validationErrors: String = ""
    
      def primaryKey = %s
}
    """ format (className, 
    			fieldList, 
    			DataAppGenerator.fieldType(primaryKeys.head),
    			defaultValues,
    			DataAppGenerator.valueName(primaryKeys.head.column))

      case false => 
    
    """    
 package models
    
 case class %s (
 %s
    ) extends Mdl[%s] {
    
      def this() = this(%s)

  	  def validate: Boolean = true
    
	  def validationErrors: String = ""
    
      def selectIdentifier: (String, String) = %s.get.toString -> %s.get.toString
    
      def compare(a: %s, b: %s) = a.%s.get.compareTo(b.%s.get)
        
      def primaryKey = %s
}
    """ format (className, fieldList, DataAppGenerator.fieldType(primaryKeys.head), defaultValues, firstPrimaryKeyValue, firstPrimaryKeyValue,
                className, className, firstPrimaryKeyValue, firstPrimaryKeyValue,
                DataAppGenerator.valueName(primaryKeys.head.column))
    }
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