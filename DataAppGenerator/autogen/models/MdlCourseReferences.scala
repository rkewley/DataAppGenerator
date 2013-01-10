    
 package models
    
 case class MdlCourseReferences (
 	vidCourseReferences : Long,
	vCourse : Long,
	vReference : Long
    )  {
    
      def this() = this(0, 0, 0)

  	  def validate: Boolean = true
    
	  def validationErrors: String = ""
    
      def selectIdentifier: (String, String) = vidCourseReferences.toString -> vidCourseReferences.toString
    
      def compare(a: MdlCourseReferences, b: MdlCourseReferences) = a.vidCourseReferences.compareTo(b.vidCourseReferences)
}
    