    
 package models
    
 case class MdlDepartments (
 	vDepartmentID : Long,
	vDepartmentName : String,
	vDepartmentHead : String,
	vDepartmentHopePage : String
    )  {
    
      def this() = this(0, "", "", "")

  	  def validate: Boolean = true
    
	  def validationErrors: String = ""
    
      def selectIdentifier: (String, String) = vDepartmentID.toString -> vDepartmentID.toString
    
      def compare(a: MdlDepartments, b: MdlDepartments) = a.vDepartmentID.compareTo(b.vDepartmentID)
}
    