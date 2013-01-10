    
 package models
    
 case class MdlUser (
 	vemail : String,
	vpassword : String,
	vpermissions : String
    )  {
    
      def this() = this("", "", "")

  	  def validate: Boolean = true
    
	  def validationErrors: String = ""
    
      def selectIdentifier: (String, String) = vemail.toString -> vemail.toString
    
      def compare(a: MdlUser, b: MdlUser) = a.vemail.compareTo(b.vemail)
}
    