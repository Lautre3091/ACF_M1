package validator.SinquinGestinRenault
import bank._

import collection.immutable.List
import math._
import common._

/* The object to complete */ 
class ConcreteValidator extends TransValidator{

	// TODO
	def process(m:message){}

	// TODO
	def getValidTrans: List[((bank.Nat, (bank.Nat, bank.Nat)),bank.Nat)]= List()

	// TODO
	def authors= "SinquinGestinRenault"

	object Natural {

		def apply(numeral: BigInt): Natural = new Natural(numeral max 0)
		def apply(numeral: Int): Natural = Natural(BigInt(numeral))
		def apply(numeral: String): Natural = Natural(BigInt(numeral))

	}

	class Natural private(private val value: BigInt) {

		override def hashCode(): Int = this.value.hashCode()

		override def equals(that: Any): Boolean = that match {
			case that: Natural => this equals that
			case _ => false
		}

		override def toString(): String = this.value.toString

		def equals(that: Natural): Boolean = this.value == that.value

		def as_BigInt: BigInt = this.value
		def as_Int: Int = if (this.value >= scala.Int.MinValue && this.value <= scala.Int.MaxValue)
			this.value.intValue
		else error("Int value out of range: " + this.value.toString)

		def +(that: Natural): Natural = new Natural(this.value + that.value)
		def -(that: Natural): Natural = Natural(this.value - that.value)
		def *(that: Natural): Natural = new Natural(this.value * that.value)

		def /%(that: Natural): (Natural, Natural) = if (that.value == 0) (new Natural(0), this)
		else {
			val (k, l) = this.value /% that.value
			(new Natural(k), new Natural(l))
		}

		def <=(that: Natural): Boolean = this.value <= that.value

		def <(that: Natural): Boolean = this.value < that.value

	}


	object tp89 {

		abstract sealed class message
		final case class Pay(a: (Nat, (Nat, Nat)), b: Nat) extends message
		final case class Ack(a: (Nat, (Nat, Nat)), b: Nat) extends message
		final case class Cancel(a: (Nat, (Nat, Nat))) extends message

		abstract sealed class etat
		final case class EnCour(a: (Nat, Nat)) extends etat
		final case object Annule extends etat
		final case class Valide(a: Nat) extends etat
		final case class PrixProposer(a: (Nat, Nat)) extends etat
		final case class PrixDemander(a: (Nat, Nat)) extends etat

		def equal(x0: (Nat, (Nat, Nat)), x1: (Nat, (Nat, Nat))): Boolean = (x0, x1)
		match {
			case ((c1, (m1, idT1)), (c2, (m2, idT2))) =>
				c1 == c2 && (m1 == m2 && idT1 == idT2)
		}

		def export(x0: List[((Nat, (Nat, Nat)), etat)]):
		List[((Nat, (Nat, Nat)), etat)] =
			x0 match {
				case Nil => Nil
				case (tId, etat) :: bdd =>
					(etat match {
						case EnCour(_) => export(bdd)
						case Annule => export(bdd)
						case Valide(_) => (tId, etat) :: export(bdd)
						case PrixProposer(_) => export(bdd)
						case PrixDemander(_) => export(bdd)
					})
			}

		def annuleOffre(tId: (Nat, (Nat, Nat)), x1: List[((Nat, (Nat, Nat)), etat)]):
		List[((Nat, (Nat, Nat)), etat)] =
			(tId, x1) match {
				case (tId, Nil) => List((tId, Annule))
				case (tId1, (tId2, etat) :: s) =>
					(if (equal(tId1, tId2)) (tId2, Annule) :: s
					else (tId2, etat) :: annuleOffre(tId2, s))
			}

		def offreClient(tId: (Nat, (Nat, Nat)), x: Nat,
										xa2: List[((Nat, (Nat, Nat)), etat)]):
		List[((Nat, (Nat, Nat)), etat)] =
			(tId, x, xa2) match {
				case (tId, x, Nil) =>
					(if (Nat(0) < x) List((tId, PrixProposer((x, Nat(0))))) else Nil)
				case (tId1, x, (tId2, etat) :: s) =>
					(if (equal(tId1, tId2))
						(etat match {
							case EnCour((pC, pM)) =>
								(if (pC < x)
									(if (pM <= x) (tId2, Valide(x)) :: s
									else (tId2, EnCour((x, pM))) :: s)
								else (tId2, etat) :: s)
							case Annule => (tId2, etat) :: s
							case Valide(_) => (tId2, etat) :: s
							case PrixProposer((pC, pM)) =>
								(if (x < pC && Nat(0) < x) (tId2, PrixProposer((x, pM))) :: s
								else (tId2, PrixProposer((pC, pM))) :: s)
							case PrixDemander((_, pM)) =>
								(if (Nat(0) < x)
									(if (pM <= x) (tId2, Valide(x)) :: s
									else (tId2, EnCour((x, pM))) :: s)
								else (tId2, etat) :: s)
						})
					else (tId2, etat) :: offreClient(tId1, x, s))
			}

		def offreMarchant(tId: (Nat, (Nat, Nat)), x: Nat,
											xa2: List[((Nat, (Nat, Nat)), etat)]):
		List[((Nat, (Nat, Nat)), etat)] =
			(tId, x, xa2) match {
				case (tId, x, Nil) =>
					(if (Nat(0) < x) List((tId, PrixDemander((Nat(0), x)))) else Nil)
				case (tId1, x, (tId2, etat) :: s) =>
					(if (equal(tId1, tId2))
						(etat match {
							case EnCour((pC, pM)) =>
								(if (x < pM)
									(if (x <= pC) (tId2, Valide(pC)) :: s
									else (tId2, EnCour((pC, x))) :: s)
								else (tId2, etat) :: s)
							case Annule => (tId2, etat) :: s
							case Valide(_) => (tId2, etat) :: s
							case PrixProposer((pC, _)) =>
								(if (Nat(0) < x)
									(if (x <= pC) (tId2, Valide(pC)) :: s
									else (tId2, EnCour((pC, x))) :: s)
								else (tId1, etat) :: s)
							case PrixDemander((pC, pM)) =>
								(if (x < pM && Nat(0) < x) (tId2, PrixDemander((pC, x))) :: s
								else (tId2, PrixDemander((pC, pM))) :: s)
						})
					else (tId2, etat) :: offreMarchant(tId1, x, s))
			}

		def traiterMessage(x0: message, bdd: List[((Nat, (Nat, Nat)), etat)]):
		List[((Nat, (Nat, Nat)), etat)] =
			(x0, bdd) match {
				case (Pay(tId, x), bdd) => offreClient(tId, x, bdd)
				case (Ack(tId, x), bdd) => offreMarchant(tId, x, bdd)
				case (Cancel(tId), bdd) => annuleOffre(tId, bdd)
			}

	} /* object tp89 */


}


