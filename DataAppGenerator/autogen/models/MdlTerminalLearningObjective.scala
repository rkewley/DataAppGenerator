    
 package models
    
 case class MdlTerminalLearningObjective (
 	vidTerminalLearningObjective : Long,
	vTerminalLearningObjective : String,
	vTopic : Long,
	vProgram : String
    )  {
    
      def this() = this(0, "", 0, "")

  	  def validate: Boolean = true
    
	  def validationErrors: String = ""
    
      def selectIdentifier: (String, String) = vidTerminalLearningObjective.toString -> vidTerminalLearningObjective.toString
    
      def compare(a: MdlTerminalLearningObjective, b: MdlTerminalLearningObjective) = a.vidTerminalLearningObjective.compareTo(b.vidTerminalLearningObjective)
}
    