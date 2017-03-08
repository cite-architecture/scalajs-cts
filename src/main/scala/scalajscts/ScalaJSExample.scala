package scalajscts

import scalajs.js
import scala.scalajs.js.annotation.JSExport
import org.scalajs.dom
import org.scalajs.dom.html
import scala.util.Random
import scalajs.js.annotation.JSExport
import scalatags.JsDom.all._
import rx._
import edu.holycross.shot.cite._
import edu.holycross.shot.ohco2._

@JSExport
object CtsExample extends {
	@JSExport
	def main(textSpace: html.Div) = {
		val urnString = "urn:cts:greekLit:tlg0012.tlg001.msA:1.1"
		//val cc = CtsCorpus("urnString")
		//val myCorpus = cc.makeDumbCorpus
		//val tempCitedWorks = myCorpus.citedWorks


/*
		textSpace.appendChild(
			div(
				ul(
					tempCitedWorks.map(w =>
						li(
							w.toString
						)
					)
				)
			).render
		)
		*/

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


		var wholeCorpus: CtsCorpus  = null

		val filePicker = input(
			`type`:="file"
		).render

		val citedWorks = Var("")

		val citedWorksDisplay = div(
				`id`:= "citedWorks",
				 citedWorks
		).render

		filePicker.onchange = (e: dom.Event) => {
			//file() = filePicker.value
			val reader = new dom.FileReader()
			reader.readAsText(filePicker.files(0))
			reader.onload = (e: dom.Event) => {
				val contents = reader.result.asInstanceOf[String]
				wholeCorpus = CtsCorpus(contents)
				citedWorks() = wholeCorpus.getCitedWorksStr
			}
		}

		textSpace.appendChild(
			filePicker
		).render


		textSpace.appendChild(
			citedWorksDisplay
		).render

	}
}
