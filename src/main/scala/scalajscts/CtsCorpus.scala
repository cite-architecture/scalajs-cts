package scalajscts
import scalajs.js
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
	 val tempCorpus: Corpus = corpus ~~ urn
	 val passageString: String = tempCorpus.contents.mkString("\n")
	 passageString
 }

 def getPrevUrnForCurrent(urn:CtsUrn): Option[CtsUrn] = {
	 	val pu = corpus.prevUrn(urn)
		pu
 }

 def getNextUrnForCurrent(urn:CtsUrn): Option[CtsUrn] = {
	 	val nu = corpus.nextUrn(urn)
		nu
 }

 def getFirstCitation(urn:CtsUrn): String = {
   val furn: CtsUrn = corpus.firstNode(urn).urn
	 furn.toString
 }

	def testMethod(urnString: String): String = {
		val urn = CtsUrn(urnString)
		urn.dropPassage.toString
	}



}
