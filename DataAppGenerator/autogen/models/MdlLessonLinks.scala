    
 package models
    
 case class MdlLessonLinks (
 	vLessonLinkNumber : Long,
	vLink : String,
	vDescription : String,
	vIsFileLiink : Boolean,
	vLesson : Long
    )  {
    
      def this() = this(0, "", "", false, 0)

  	  def validate: Boolean = true
    
	  def validationErrors: String = ""
    
      def selectIdentifier: (String, String) = vLessonLinkNumber.toString -> vLessonLinkNumber.toString
    
      def compare(a: MdlLessonLinks, b: MdlLessonLinks) = a.vLessonLinkNumber.compareTo(b.vLessonLinkNumber)
}
    