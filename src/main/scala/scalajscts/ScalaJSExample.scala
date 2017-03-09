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

		// Bound Stuff
		val citedWorks = Var("")
		val citedWorksHTML = Rx{

					citedWorks().split("\n").map( cw =>
						ul(
							li(
								onclick:="getElementById('urnTextInput').value = this.textContent;",
								cw
							)
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


		//val urnSubmit = input(
		//		`type`:="submit"
		//)

		urnTextInput.onchange = (e: dom.Event) => {
			try {
				typedUrn = CtsUrn(urnTextInput.value)
				println(typedUrn.passageComponentOption)
				document.getElementById("validUrnFlag").className = "validUrn"
				if (typedUrn.passageComponentOption != None){
					currentPassage() = wholeCorpus.getCtsText(typedUrn)
				} else {
					currentPassage() ="No passage component in URN"
				}
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
				"URN: ",
				urnTextInput,
				span(
						`id`:= "validUrnFlag"
				)
			)
		).render

		val filePicker = input(
			`type`:="file"
		).render

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
				currentPassageDisplay
		).render

		textSpace.appendChild(
			citedWorksDisplay
		).render

		textSpace.appendChild(
			filePicker
		).render

		// On initialization, go ahead and load via AJAX the default library
		println(defaultLibraryUrl)

		displayMessage("Loading remote library…",false)
		Ajax.get(defaultLibraryUrl).onSuccess { case xhr =>
			wholeCorpus = CtsCorpus(xhr.responseText)
			citedWorks() = wholeCorpus.getCitedWorksStr
			clearMessage
		}


	}
}
