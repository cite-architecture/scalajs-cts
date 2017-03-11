package scalajscts

import scalajs.js
import scala.scalajs.js.annotation.JSExport
import org.scalajs.dom
import org.scalajs.dom.html
import org.scalajs.dom.ext.Ajax
import scala.util.Random
import scalajs.js.annotation.JSExport
import scalatags.JsDom.all._
import rx._
import edu.holycross.shot.cite._
import edu.holycross.shot.ohco2._
import js.Dynamic.{global => g}
import scala.concurrent
              .ExecutionContext
              .Implicits
              .global

@JSExport
object CtsExample extends {
	@JSExport
	def main(textSpace: html.Div, defaultLibraryUrl: String) = {


		// Basic messaging
		val document = js.Dynamic.global.document

		def clearMessage: Unit = { document.getElementById("messages").innerHTML = "" }

		def displayMessage(msg: String, warn: Boolean): Unit = {
			if (warn){
				document.getElementById("messages").className = "red"
			} else {
				document.getElementById("messages").className = "green"
			}
			document.getElementById("messages").innerHTML = msg
		}


		// Binding magic. Never touch this!
		implicit def rxFrag[T <% Frag](r: Rx[T]): Frag = {
			def rSafe: dom.Node = span(r()).render
			var last = rSafe
			Obs(r, skipInitial = true){
				val newLast = rSafe
				js.Dynamic.global.last = last
				last.parentNode.replaceChild(newLast, last)
				last = newLast
			}
			last
		}
		// End Binding magic


		// Persistent, app-wide values for updating
		var wholeCorpus: CtsCorpus  = null
		var urnString: String = ""
		var typedUrn: CtsUrn = null
		var currentUrn: CtsUrn = null
		var currentNext: Option[CtsUrn] = None
		var currentPrev: Option[CtsUrn] = None


		// Bound Stuff

		val currentUrnBound = Var(currentUrn)
		val currentPrevBound = Var(currentPrev)
		val currentNextBound = Var(currentNext)

		val currentMoreBound = Rx{
			val tempString = "test"
			tempString
		}

		val currentPassage = Var("")
		val currentPassageHTML = Rx{
				currentPassage().split("\n").map( cp =>
					p(
						cp
					)
				)
		}

		def createCitedWorksListItem(us: String) = {
		  val l = li( us ).render
			l.onclick = (_: dom.Event) => {
				try{
					val workUrn = CtsUrn(us)
					val firstUrnString = wholeCorpus.getFirstCitation(workUrn)
					document.getElementById("urnTextInput").value = firstUrnString
					currentPassage() = ""
				} catch {
					case e: Exception => document.getElementById("urnTextInput").value = "No first urn found."
				}
			}
			l
		}

		val citedWorks = Var("")
		val citedWorksHTML = Rx{

				ul(
					citedWorks().split("\n").map( cw =>
							createCitedWorksListItem(cw)
						)
				)
		}



		val urnTextInput = input(
			`id`:="urnTextInput",
			`type`:="text",
			`size`:="50",
			`class`:="invalidUrn"
		).render

		val urnTextInputSubmitButton = input(
			`id`:= "getTextButton",
		  `type`:= "submit"
		).render

		def fetchText: Unit = {
					currentUrnBound() = currentUrn
					currentPassage() = wholeCorpus.getCtsText(currentUrn)
					document.getElementById("urnTextInput").value = currentUrn.toString
		}

		def updateText: Unit = {
			displayMessage("Fetching text…", false)
			g.console.log(s"updating text with: ${currentUrn}")
			val timeStart = new js.Date().getTime()
			fetchText
			currentPrevBound() = wholeCorpus.getPrevUrnForCurrent(currentUrnBound())
			currentNextBound() = wholeCorpus.getNextUrnForCurrent(currentUrnBound())
			val timeEnd = new js.Date().getTime()
			displayMessage(s"Fetched text in ${(timeEnd - timeStart)/1000} seconds.",false)
		}

/*
		urnTextInput.onchange = (e: dom.Event) => {
			try {
				typedUrn = CtsUrn(urnTextInput.value)
				document.getElementById("validUrnFlag").className = "validUrn"
				if (typedUrn.passageComponentOption != None){
					currentUrn = typedUrn
				} else {
					currentPassage() ="No passage component in URN"
				}
				updateText
			} catch {
				case e: Exception => document.getElementById("validUrnFlag").className = "invalidUrn"
			}
		}
		*/

		urnTextInputSubmitButton.onclick = (_: dom.Event) => {
			try {
				typedUrn = CtsUrn(urnTextInput.value)
				document.getElementById("validUrnFlag").className = "validUrn"
				if (typedUrn.passageComponentOption != None){
					currentUrn = typedUrn
				} else {
					currentPassage() ="No passage component in URN"
				}
				updateText
			} catch {
				case e: Exception => document.getElementById("validUrnFlag").className = "invalidUrn"
			}
		}

		urnTextInput.onkeyup = (e: dom.Event) => {
			try {
				typedUrn = CtsUrn(urnTextInput.value)
				document.getElementById("validUrnFlag").className = "validUrn"
			} catch {
				case e: Exception => document.getElementById("validUrnFlag").className = "invalidUrn"
			 }
		}

		val urnField = (
			div(
				`id`:="urnField",
				label(
					`for`:= "urnTextInput",
					"URN"
				),
				urnTextInput,
				urnTextInputSubmitButton,
				span(
						`id`:= "validUrnFlag"
				)
			)
		).render

		val filePicker = input(
			`type`:="file"
		).render

		val nextButton = button(
			`class`:="navButton",
			"→"
		).render

		val prevButton = button(
			`class`:="navButton",
			"←"
		).render



		val prevButtonVisibility = Rx{
				if (currentPrevBound() == None){
					prevButton.setAttribute("class","navButton hide")
				} else {
					prevButton.setAttribute("class","navButton")
				}
			}

		val nextButtonVisibility = Rx{
				if (currentNextBound() == None){
					nextButton.setAttribute("class","navButton hide")
				} else {
					nextButton.setAttribute("class","navButton")
				}
			}



		nextButton.onclick = (e: dom.Event) => {
			val nextUrn = wholeCorpus.corpus.nextUrn(currentUrnBound())
			if (nextUrn != None){
				currentUrn = nextUrn.get
				updateText
			}
		}

		prevButton.onclick = (e: dom.Event) => {
			val prevUrn = wholeCorpus.corpus.prevUrn(currentUrnBound())
			if (prevUrn != None){
				currentUrn = prevUrn.get
				updateText
			}
		}


		val currentPassageDisplay = div(
				`id`:= "currentPassage",
				 currentPassageHTML
		).render

		val citedWorksDisplay = div(
				`id`:= "citedWorks",

				 h3(
					 "Cited Works"
				 ),
				 citedWorksHTML
		).render

	val nGramSubmit = input(
					`id`:="ngramSubmit",
					`type`:= "Submit",
					"Get N-Gram"
		).render

	val nGramScopeSelect = Rx{
			if (currentUrnBound() == null){
				select(
					`id`:= "nGramScopeOption",
					option( `value`:= "corpus", "Whole Corpus")
				).render
			} else {
				select(
					`id`:= "nGramScopeOption",
					option( `value`:= "current", "Current Text" ),
					option( `value`:= "corpus", "Whole Corpus")
				).render
			}
	}


		nGramSubmit.onclick = (_: dom.Event) => {

			// Get All the Variables
				val n:Int = document.getElementById("nlist").value.toString.toInt
				val occ:Int = document.getElementById("minOccurrances").value.toString	.toInt


			if (wholeCorpus == null){
				displayMessage("No library loaded.",true)
			} else {
				g.console.log("about to do ngram…")
				document.getElementById("nGramScopeOption").value.toString match {
					case "current" => {
						g.console.log("…on current text")
						//g.console.log(wholeCorpus.getNGram(currentUrn.dropPassage))
						document.getElementById("nGramSpace").innerHTML = wholeCorpus.getNGram(currentUrn.dropPassage)
					}
					case _ => {
						g.console.log("…on whole corpus")
						document.getElementById("nGramSpace").innerHTML = wholeCorpus.getNGram
					}
				}
			}
		}

	 val nGramControls = div(
		 `id`:="nGramControls",
		 h3(
			 "Analytical Tools"
		 ),
				label(
					`for`:= "nlist",
					"N-Gram"
				),
				select(
					`id`:="nlist",
					 option( `value`:= "1", "1" ),
					 option( `value`:= "2", "2" ),
					 option( `value`:= "3", "3" ),
					 option( `value`:= "4", "4" ),
					 option( `value`:= "5", "5" ),
					 option( `value`:= "6", "6" ),
					 option( `value`:= "7", "7" ),
					 option( `value`:= "8", "8" )
				),
				label(
					`for`:="minOccurrances",
					"Occurs"
				),
				select(
					`id`:="minOccurrances",
					 option( `value`:= "1", "1" ),
					 option( `value`:= "2", "2" ),
					 option( `value`:= "3", "3" ),
					 option( `value`:= "4", "4" ),
					 option( `value`:= "5", "5" ),
					 option( `value`:= "6", "6" ),
					 option( `value`:= "7", "7" ),
					 option( `value`:= "8", "8" )
				),
				br,
				nGramScopeSelect,
				br,
			  label(
					`for`:="filterStringField",
					"Filter String"
				),
			 	input(
					`type`:= "text",
					`size`:= "20",
					`id`:= "filterStringField"
				),
				br,
				label(
					`for`:="ignorePuncBox",
					"Ignore Punctuation"
				),
				input(
					`type`:="checkbox",
					`id`:="ignorePuncBox",
					`checked`:="true"
				),
				br,
				nGramSubmit,
				br,
				"(Finding N-Grams on large corpora can take many seconds.)"


	 ).render

	 val utilityDiv = div(
		 `id`:= "utilityFields",
			nGramControls,
		  citedWorksDisplay
	 ).render


		filePicker.onchange = (e: dom.Event) => {
			//file() = filePicker.value
			displayMessage("Loading local library…", false)
			val reader = new dom.FileReader()
			reader.readAsText(filePicker.files(0))
			reader.onload = (e: dom.Event) => {
				val contents = reader.result.asInstanceOf[String]
				wholeCorpus = CtsCorpus(contents)
				citedWorks() = wholeCorpus.getCitedWorksStr
				clearMessage
			}
		}

		textSpace.appendChild(
			urnField
		).render

		textSpace.appendChild(
			utilityDiv
		).render


		textSpace.appendChild(
			filePicker
		).render


		textSpace.appendChild(
				currentPassageDisplay
		).render

		currentPassageDisplay.appendChild(
			prevButton
		).render


		currentPassageDisplay.appendChild(
			nextButton
		).render


/*
		textSpace.appendChild(
			filePicker
		).render
		*/

		// On initialization, go ahead and load via AJAX the default library
		// …how does one check for failure?

/*
		displayMessage("Loading remote library…",false)
		val remoteCall = Ajax.get(defaultLibraryUrl).onSuccess { case xhr =>
				wholeCorpus = CtsCorpus(xhr.responseText)
				citedWorks() = wholeCorpus.getCitedWorksStr
				clearMessage
		}
		*/



	}
}
