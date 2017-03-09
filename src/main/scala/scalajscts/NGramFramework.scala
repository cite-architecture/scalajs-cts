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
object NGrammer extends {
	@JSExport
	def main(nGramSpace: html.Div) = {

		// Basic messaging
		val document = js.Dynamic.global.document

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

		val testPara = p(
			"N-Grams go here."
		).render

		nGramSpace.appendChild(
			testPara
		).render


	}
}
