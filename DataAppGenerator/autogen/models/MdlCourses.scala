    
 package models
    
 case class MdlCourses (
 	vidCourse : Long,
	vCourseIDNumber : String,
	vAcademicYear : Long,
	vAcademicTerm : Long,
	vCourseName : String,
	vCourseDirectorEmail : String,
	vProgramDirectorEmail : String,
	vCourseDescriptionRedbook : String,
	vCreditHours : Double,
	vPrerequisites : String,
	vCorequisites : String,
	vDisqualifiers : String,
	vCourseStrategy : String,
	vCriteriaForPassing : String,
	vAdminInstructions : String,
	vDepartmentID : Long,
	vCourseWebsite : Boolean,
	vCourseDescriptionWebsite : String
    )  {
    
      def this() = this(0, "", 0, 0, "", "", "", "", 0.0, "", "", "", "", "", "", 0, false, "")

  	  def validate: Boolean = true
    
	  def validationErrors: String = ""
    
      def selectIdentifier: (String, String) = vidCourse.toString -> vidCourse.toString
    
      def compare(a: MdlCourses, b: MdlCourses) = a.vidCourse.compareTo(b.vidCourse)
}
    