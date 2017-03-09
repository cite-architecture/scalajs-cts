package scalajscts
import edu.holycross.shot.cite._
import edu.holycross.shot.ohco2._

case class CtsCorpus(twocol: String){

 val corpus = Corpus(twocol,"\t")

 def getCitedWorksStr: String = {
	 val urns = corpus.citedWorks
	 val urnsStr = urns.map( u => u.toString + "\n")
	 urnsStr.mkString
 }

 def getCtsText(urn:CtsUrn): String = {
	 //val ctsText:String = corpus.textContents(urn,"\n")
	 val tempCorpus: Corpus = corpus ~~ urn
	 val passageString: String = tempCorpus.contents.mkString("\n")
	 passageString
 }

	def testMethod(urnString: String): String = {
		val urn = CtsUrn(urnString)
		urn.dropPassage.toString
	}



}
