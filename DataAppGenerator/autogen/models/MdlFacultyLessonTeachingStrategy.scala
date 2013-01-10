    
 package models
    
 case class MdlFacultyLessonTeachingStrategy (
 	vFaculty_Email : String,
	vLessons_LessonIndex : Long,
	vTeachingStrategy : String
    )  {
    
      def this() = this("", 0, "")

  	  def validate: Boolean = true
    
	  def validationErrors: String = ""
    
      def selectIdentifier: (String, String) = vFaculty_Email.toString -> vFaculty_Email.toString
    
      def compare(a: MdlFacultyLessonTeachingStrategy, b: MdlFacultyLessonTeachingStrategy) = a.vFaculty_Email.compareTo(b.vFaculty_Email)
}
    