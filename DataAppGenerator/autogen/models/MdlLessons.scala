    
 package models
    
 case class MdlLessons (
 	vLessonIndex : Long,
	vLessonNumber : Long,
	vLessonName : String,
	vAssignment : String,
	vLocation : String,
	vidCourse : Long,
	vDuration : Long,
	vLab : Boolean,
	vLessonSummary : String
    )  {
    
      def this() = this(0, 0, "", "", "", 0, 0, false, "")

  	  def validate: Boolean = true
    
	  def validationErrors: String = ""
    
      def selectIdentifier: (String, String) = vLessonIndex.toString -> vLessonIndex.toString
    
      def compare(a: MdlLessons, b: MdlLessons) = a.vLessonIndex.compareTo(b.vLessonIndex)
}
    