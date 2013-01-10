    
 package models
    
 case class MdlPrograms (
 	vProgram : String,
	vName : String,
	vSlogan : String,
	vInformation : String,
	vProgramDirector : String,
	vDepartment : Long
    )  {
    
      def this() = this("", "", "", "", "", 0)

  	  def validate: Boolean = true
    
	  def validationErrors: String = ""
    
      def selectIdentifier: (String, String) = vProgram.toString -> vProgram.toString
    
      def compare(a: MdlPrograms, b: MdlPrograms) = a.vProgram.compareTo(b.vProgram)
}
    