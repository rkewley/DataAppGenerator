    
 package models
    
 case class MdlLessonTopicObjectives (
 	vidLessonTopicObjectives : Long,
	vLesson : Long,
	vTopicObjective : Long
    )  {
    
      def this() = this(0, 0, 0)

  	  def validate: Boolean = true
    
	  def validationErrors: String = ""
    
      def selectIdentifier: (String, String) = vidLessonTopicObjectives.toString -> vidLessonTopicObjectives.toString
    
      def compare(a: MdlLessonTopicObjectives, b: MdlLessonTopicObjectives) = a.vidLessonTopicObjectives.compareTo(b.vidLessonTopicObjectives)
}
    