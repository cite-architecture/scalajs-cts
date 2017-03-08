package scalajscts
import edu.holycross.shot.cite._
import edu.holycross.shot.ohco2._

case class CtsCorpus(twocol: String){

val dumbCorpus = """urn:cts:greekLit:tlg0012.tlg001.msA:1.1#<l xmlns="http://www.tei-c.org/ns/1.0" n="1"> Μῆνιν ἄειδε θεὰ <persName n="urn:cite2:hmt:pers.r1:pers1"> Πηληϊάδεω Ἀχιλῆος</persName></l>
urn:cts:greekLit:tlg0012.tlg001.msA:1.18#<l xmlns="http://www.tei-c.org/ns/1.0" n="18"> ὑμῖν μὲν θεοὶ δοῖεν <placeName n="urn:cite2:hmt:place.r1:place45"> Ὀλύμπια</placeName> δώματ' ἔχοντες </l>
urn:cts:greekLit:tlg5026.msAint.hmt:19.hc_3.lemma#<div xmlns="http://www.tei-c.org/ns/1.0" n="lemma"> <p/></div>
urn:cts:greekLit:tlg5026.msAint.hmt:19.hc_3.comment#<div xmlns="http://www.tei-c.org/ns/1.0" n="comment"> <p> καὶ <q> πόρε <persName n="urn:cite2:hmt:pers.r1:pers115"> Xείρων</persName></q> ⁑</p></div>
urn:cts:greekLit:tlg5026.msAint.hmt:19.hc_4.lemma#<div xmlns="http://www.tei-c.org/ns/1.0" n="lemma"> <p/></div>
urn:cts:greekLit:tlg5026.msAint.hmt:19.hc_4.comment#<div xmlns="http://www.tei-c.org/ns/1.0" n="comment"> <p> <choice> <abbr> ουτ</abbr> <expan> οὕτως</expan></choice> πληθυντικῶς αι <persName n="urn:cite2:hmt:pers.r1:pers16"> <choice> <abbr> Αρισταρχ</abbr> <expan> Ἀριστάρχου</expan></choice></persName> ⁑</p></div>
urn:cts:greekLit:tlg5026.msAint.hmt:19.hc_5.lemma#<div xmlns="http://www.tei-c.org/ns/1.0" n="lemma"> <p/></div>
urn:cts:greekLit:tlg5026.msAint.hmt:19.hc_5.comment#<div xmlns="http://www.tei-c.org/ns/1.0" n="comment"> <p> <choice> <abbr> ουτ</abbr> <expan> οὕτως</expan></choice> δια τοῦ <rs type="waw"> ο</rs> <q> ζεύγνυον</q> ⁑</p></div>
"""

	def makeDumbCorpus: Corpus = {
		Corpus (dumbCorpus,"#")
	}

 val corpus = Corpus(twocol,"\t")

 def getCitedWorksStr: String = {
	 val urns = corpus.citedWorks
	 val urnsStr = urns.map( u => u.toString + "\n")
	 urnsStr.mkString
 }

	def testMethod(urnString: String): String = {
		val urn = CtsUrn(urnString)
		urn.dropPassage.toString
	}



}
