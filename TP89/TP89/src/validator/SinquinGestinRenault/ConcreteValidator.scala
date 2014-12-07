package validator.GestinSinquinRenault

import validator.GestinSinquinRenault.tp89._

import bank._

class ConcreteValidator extends bank.TransValidator{

  var bdd : List[(etat, ((Nat, (Nat, Nat)), (Nat, Nat)))] = List()
  def process(m:bank.message){bdd = tp89.traiterMessage(Converter.scalaMessageToIsabelle(m), bdd)}
  def getValidTrans: List[((bank.Nat, (bank.Nat, bank.Nat)), bank.Nat)] = {Converter.isabelleExportListToScalaExportList(tp89.export(bdd))}

  // TODO
  def authors= "SinquinGestinRenault"
}

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
final case object Validee extends etat
final case object EnCours extends etat
final case object Annulee extends etat
final case object PrixProposer extends etat
final case object PrixDemander extends etat

def equal(x0: (Nat, (Nat, Nat)), x1: (Nat, (Nat, Nat))): Boolean = (x0, x1)
  match {
  case ((c1, (m1, idT1)), (c2, (m2, idT2))) =>
    c1 == c2 && (m1 == m2 && idT1 == idT2)
}

def equal_etat(x0: etat, x1: etat): Boolean = (x0, x1) match {
  case (PrixDemander, PrixProposer) => false
  case (PrixProposer, PrixDemander) => false
  case (PrixDemander, Annulee) => false
  case (Annulee, PrixDemander) => false
  case (PrixProposer, Annulee) => false
  case (Annulee, PrixProposer) => false
  case (PrixDemander, EnCours) => false
  case (EnCours, PrixDemander) => false
  case (PrixProposer, EnCours) => false
  case (EnCours, PrixProposer) => false
  case (Annulee, EnCours) => false
  case (EnCours, Annulee) => false
  case (PrixDemander, Validee) => false
  case (Validee, PrixDemander) => false
  case (PrixProposer, Validee) => false
  case (Validee, PrixProposer) => false
  case (Annulee, Validee) => false
  case (Validee, Annulee) => false
  case (EnCours, Validee) => false
  case (Validee, EnCours) => false
  case (PrixDemander, PrixDemander) => true
  case (PrixProposer, PrixProposer) => true
  case (Annulee, Annulee) => true
  case (EnCours, EnCours) => true
  case (Validee, Validee) => true
}

def export(x0: List[(etat, ((Nat, (Nat, Nat)), (Nat, Nat)))]):
      List[((Nat, (Nat, Nat)), Nat)] =
  x0 match {
  case Nil => Nil
  case (etat, (tId, (pClient, pMarchand))) :: s =>
    (if (equal_etat(etat, Validee)) (tId, pClient) :: export(s) else export(s))
}

def reverse[A](x0: List[A]): List[A] = x0 match {
  case Nil => Nil
  case x :: xs => reverse[A](xs) ++ List(x)
}

def annuleOffre(tId: (Nat, (Nat, Nat)),
                 x1: List[(etat, ((Nat, (Nat, Nat)), (Nat, Nat)))]):
      List[(etat, ((Nat, (Nat, Nat)), (Nat, Nat)))] =
  (tId, x1) match {
  case (tId, Nil) => List((Annulee, (tId, (Nat(0), Nat(0)))))
  case (tId1, (etat, (tId2, (offre, demande))) :: s) =>
    (if (equal(tId1, tId2)) (Annulee, (tId2, (offre, demande))) :: s
      else (etat, (tId2, (offre, demande))) :: annuleOffre(tId1, s))
}

def offreClient(tId: (Nat, (Nat, Nat)), x: Nat,
                 xa2: List[(etat, ((Nat, (Nat, Nat)), (Nat, Nat)))]):
      List[(etat, ((Nat, (Nat, Nat)), (Nat, Nat)))] =
  (tId, x, xa2) match {
  case (tId, x, Nil) =>
    (if (Nat(0) < x) List((PrixProposer, (tId, (x, Nat(0))))) else Nil)
  case (tId1, x, (etat, (tId2, (pClient, pMarchand))) :: s) =>
    (if (equal(tId1, tId2))
      (etat match {
         case Validee => (etat, (tId2, (pClient, pMarchand))) :: s
         case EnCours =>
           (if (pClient < x)
             (if (pMarchand <= x) (Validee, (tId2, (x, pMarchand))) :: s
               else (etat, (tId2, (x, pMarchand))) :: s)
             else (etat, (tId2, (pClient, pMarchand))) :: s)
         case Annulee => (etat, (tId2, (pClient, pMarchand))) :: s
         case PrixProposer =>
           (if (pClient < x) (etat, (tId2, (x, pMarchand))) :: s
             else (etat, (tId2, (pClient, pMarchand))) :: s)
         case PrixDemander =>
           (if (Nat(0) < x)
             (if (pMarchand <= x) (Validee, (tId2, (x, pMarchand))) :: s
               else (EnCours, (tId2, (x, pMarchand))) :: s)
             else (etat, (tId2, (pClient, pMarchand))) :: s)
       })
      else (etat, (tId2, (pClient, pMarchand))) :: offreClient(tId1, x, s))
}

def offreMarchand(tId: (Nat, (Nat, Nat)), x: Nat,
                   xa2: List[(etat, ((Nat, (Nat, Nat)), (Nat, Nat)))]):
      List[(etat, ((Nat, (Nat, Nat)), (Nat, Nat)))] =
  (tId, x, xa2) match {
  case (tId, x, Nil) =>
    (if (Nat(0) < x) List((PrixDemander, (tId, (Nat(0), x)))) else Nil)
  case (tId1, x, (etat, (tId2, (pClient, pMarchand))) :: s) =>
    (if (equal(tId1, tId2))
      (etat match {
         case Validee => (etat, (tId2, (pClient, pMarchand))) :: s
         case EnCours =>
           (if (x < pMarchand)
             (if (x <= pClient) (Validee, (tId2, (pClient, x))) :: s
               else (etat, (tId2, (pClient, x))) :: s)
             else (etat, (tId2, (pClient, pMarchand))) :: s)
         case Annulee => (etat, (tId2, (pClient, pMarchand))) :: s
         case PrixProposer =>
           (if (Nat(0) < x)
             (if (x <= pClient) (Validee, (tId2, (pClient, x))) :: s
               else (EnCours, (tId2, (pClient, x))) :: s)
             else (etat, (tId2, (pClient, pMarchand))) :: s)
         case PrixDemander =>
           (if (x < pMarchand && Nat(0) < x) (etat, (tId2, (pClient, x))) :: s
             else (etat, (tId2, (pClient, pMarchand))) :: s)
       })
      else (etat, (tId2, (pClient, pMarchand))) :: offreMarchand(tId1, x, s))
}

def traiterMessage(x0: message,
                    bdd: List[(etat, ((Nat, (Nat, Nat)), (Nat, Nat)))]):
      List[(etat, ((Nat, (Nat, Nat)), (Nat, Nat)))] =
  (x0, bdd) match {
  case (Pay(tId, x), bdd) => offreClient(tId, x, bdd)
  case (Ack(tId, x), bdd) => offreMarchand(tId, x, bdd)
  case (Cancel(tId), bdd) => annuleOffre(tId, bdd)
}

def traiterMessageList2(x0: List[message]):
      List[(etat, ((Nat, (Nat, Nat)), (Nat, Nat)))] =
  x0 match {
  case Nil => Nil
  case m :: s => traiterMessage(m, traiterMessageList2(s))
}

def traiterMessageList(l: List[message]):
      List[(etat, ((Nat, (Nat, Nat)), (Nat, Nat)))] =
  traiterMessageList2(reverse[message](l))

} /* object tp89 */

object Converter {

  private def scalaNatToIsabelle(n : bank.Nat): Nat = {
      Nat(n.as_BigInt)
  }
  
  private def isabelleExportEntryToScala(e : ((Nat, (Nat, Nat)), Nat)) : ((bank.Nat, (bank.Nat, bank.Nat)),bank.Nat) = {
    ((bank.Nat(e._1._1.as_BigInt), (bank.Nat(e._1._2._1.as_BigInt), bank.Nat(e._1._2._2.as_BigInt))),bank.Nat(e._2.as_BigInt))
  }

  /**
   * Transforms a bank.message into an Isabelle message
   */
  def scalaMessageToIsabelle(m : bank.message): validator.GestinSinquinRenault.tp89.message = {  
      m match {
      case bank.Pay((a1,(a2,a3)), b) => 
        validator.GestinSinquinRenault.tp89.Pay((scalaNatToIsabelle(a1),(scalaNatToIsabelle(a2),scalaNatToIsabelle(a3))),scalaNatToIsabelle(b))
      case bank.Ack((a1,(a2,a3)), b) => 
        validator.GestinSinquinRenault.tp89.Ack((scalaNatToIsabelle(a1),(scalaNatToIsabelle(a2),scalaNatToIsabelle(a3))),scalaNatToIsabelle(b))
      case bank.Cancel((a1,(a2,a3))) => 
        validator.GestinSinquinRenault.tp89.Cancel((scalaNatToIsabelle(a1),(scalaNatToIsabelle(a2),scalaNatToIsabelle(a3))))
      }
  }
  
  /**
   * Transforms the result of the Isabelle export function into the expected Scala export type
   */
  def isabelleExportListToScalaExportList(l : List[((Nat, (Nat, Nat)), Nat)]): List[((bank.Nat, (bank.Nat, bank.Nat)),bank.Nat)] = {   
    l.map(e => isabelleExportEntryToScala(e))
  }
}
