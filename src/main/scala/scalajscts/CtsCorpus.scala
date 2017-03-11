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

 def getNGram: String = {
	 getNGram(corpus)
 }

 def getNGram(ngUrn: CtsUrn): String = {
	 val newCorpus: Corpus = corpus ~~ ngUrn
	 getNGram(newCorpus)
 }

  def getNGram(ngCorpus:Corpus): String = {
			val n: Int = 4
			val thresh: Int = 8
			val punc: Boolean = true


			val timeStart = new js.Date().getTime()

			println("doing ngram")

			val hist: StringHistogram = ngCorpus.ngramHisto(n, thresh, punc)
			println(hist)
			println(hist.size)
			val strHist: String = hist.toString

			val timeEnd = new js.Date().getTime()
			println(s"Fetched N-Gram in ${(timeEnd - timeStart)/1000} seconds.")
			strHist
	}



}
