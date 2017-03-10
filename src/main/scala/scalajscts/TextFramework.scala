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

		// Testing shared values
		// g.console.log("Test")

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
		var currentNext: CtsUrn = null
		var currentPrev: CtsUrn = null

		def createCitedWorksListItem(us: String) = {
		  val l = li( us ).render
			l.onclick = (_: dom.Event) => {
				try{
					val workUrn = CtsUrn(us)
					val firstUrnString = wholeCorpus.getFirstCitation(workUrn)
					document.getElementById("urnTextInput").value = firstUrnString
				} catch {
					case e: Exception => document.getElementById("urnTextInput").value = "No first urn found."
				}
			}
			l
		}

		// Bound Stuff

		val currentUrnBound = Var(currentUrn)

		val citedWorks = Var("")
		val citedWorksHTML = Rx{

					citedWorks().split("\n").map( cw =>
						ul(
							createCitedWorksListItem(cw)
						)
					)
		}

		val currentPassage = Var("")
		val currentPassageHTML = Rx{
				currentPassage().split("\n").map( cp =>
					p(
						cp
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
			try {
				typedUrn = CtsUrn(urnTextInput.value)
				document.getElementById("validUrnFlag").className = "validUrn"
				if (typedUrn.passageComponentOption != None){
					currentUrn = typedUrn
					currentUrnBound() = currentUrn
					currentPassage() = wholeCorpus.getCtsText(currentUrn)
					document.getElementById("urnTextInput").value = currentUrn.toString
				} else {
					currentPassage() ="No passage component in URN"
				}
			} catch {
				case e: Exception => document.getElementById("validUrnFlag").className = "invalidUrn"
			}
		}

		def updateText: Unit = {
			displayMessage("Fetching text…", false)
			val timeStart = new js.Date().getSeconds()
			fetchText
			g.console.log(s"New currentUrn = ${currentUrnBound()}")
			val timeEnd = new js.Date().getSeconds()
			displayMessage(s"Fetched text in ${timeEnd - timeStart} seconds.",false)
		}

		urnTextInput.onchange = (e: dom.Event) => {
			updateText
		}

		urnTextInputSubmitButton.onclick = (_: dom.Event) => {
			updateText
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
				"URN: ",
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

		val moreButton = button(
			`class`:="navButton",
			"Show More Text"
		).render

			val moreButtonVisibility = Rx{
				if (currentUrnBound() == null){
					moreButton.setAttribute("class","navButton hide")
				} else {
					moreButton.setAttribute("class","navButton")
				}
			}

		nextButton.onclick = (e: dom.Event) => {
			g.console.log("get next")
			val nextUrn = wholeCorpus.corpus.nextUrn(currentUrnBound())
			g.console.log(s"would get ${nextUrn}")
		}

		prevButton.onclick = (e: dom.Event) => {
			g.console.log("get prev")
		}

		moreButton.onclick = (e: dom.Event) => {
			g.console.log("get more")
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
			citedWorksDisplay
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
			moreButton
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

		displayMessage("Loading remote library…",false)
		Ajax.get(defaultLibraryUrl).onSuccess { case xhr =>
			wholeCorpus = CtsCorpus(xhr.responseText)
			citedWorks() = wholeCorpus.getCitedWorksStr
			clearMessage
		}


	}
}
