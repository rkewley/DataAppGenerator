    
 package models
    
 case class MdlStudents (
 	vidStudent : Long,
	vEmail : String,
	vLastName : String,
	vFirstName : String,
	vPreferredFirstName : String,
	vCadetCompany : String,
	vMajor : String,
	vBarracksRoom : String,
	vCellPhone : String,
	vHomeTown : String,
	vCadetSports : String,
	vCadetActivities : String,
	vPlannedBranch : String,
	vPlannedPost : String,
	vExpectationsFromCourse : String,
	vTopicsToDiscuss : String
    )  {
    
      def this() = this(0, "", "", "", "", "", "", "", "", "", "", "", "", "", "", "")

  	  def validate: Boolean = true
    
	  def validationErrors: String = ""
    
      def selectIdentifier: (String, String) = vidStudent.toString -> vidStudent.toString
    
      def compare(a: MdlStudents, b: MdlStudents) = a.vidStudent.compareTo(b.vidStudent)
}
    