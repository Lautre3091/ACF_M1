package bank
import bank.observer.Observer
import bank.observer.Subject

import math._
import collection.immutable.List



/* message type coming from Isabelle Theories */

abstract sealed class message
final case class Pay(a: (Nat, (Nat, Nat)), b: Nat) extends message
final case class Ack(a: (Nat, (Nat, Nat)), b: Nat) extends message
final case class Cancel(a: (Nat, (Nat, Nat))) extends message

/* Nat Types coming from Isabelle Theories */

object Nat {
  def apply(numeral: BigInt): Nat = new Nat(numeral max 0)
  def apply(numeral: Int): Nat = Nat(BigInt(numeral))
  def apply(numeral: String): Nat = Nat(BigInt(numeral))
}

class Nat private (private val value: BigInt) {

  override def hashCode(): Int = this.value.hashCode()

  override def equals(that: Any): Boolean = that match {
    case that: Nat => this equals that
    case _ => false
  }

  override def toString(): String = this.value.toString

  def equals(that: Nat): Boolean = this.value == that.value

  def as_BigInt: BigInt = this.value
  def as_Int: Int = if (this.value >= scala.Int.MinValue && this.value <= scala.Int.MaxValue)
    this.value.intValue
  else error("Int value out of range: " + this.value.toString)

  def +(that: Nat): Nat = new Nat(this.value + that.value)
  def -(that: Nat): Nat = Nat(this.value - that.value)
  def *(that: Nat): Nat = new Nat(this.value * that.value)

  def /%(that: Nat): (Nat, Nat) = if (that.value == 0) (new Nat(0), this)
  else {
    val (k, l) = this.value /% that.value
    (new Nat(k), new Nat(l))
  }

  def <=(that: Nat): Boolean = this.value <= that.value

  def <(that: Nat): Boolean = this.value < that.value

}

object Natural {

  def apply(numeral: BigInt): Natural = new Natural(numeral max 0)
  def apply(numeral: Int): Natural = Natural(BigInt(numeral))
  def apply(numeral: String): Natural = Natural(BigInt(numeral))

}

class Natural private (private val value: BigInt) {

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


/* The bank object used in IHM */

class Bank(validator : TransValidator) extends Subject{
	var trace: List[ExtMessage]=List()
	var obs: Set[Observer]=Set()
	
	def addObserver(o:Observer)= {obs=obs + o}
	def removeObserver(o:Observer)={obs=obs - o}
	def update= obs.map((o:Observer)=> o.myNotify(this))
		
	def transListToString(l:List[((Nat, (Nat,Nat)),Nat)]):String=
	  l match {
	  case Nil => ""
	  case ((c, (m, i)),am)::rem => "(("+c+","+m+","+i+"),"+am+")\n"+transListToString(rem)
	}
	  
	def extMessageListToString(l:List[ExtMessage]):String=
	  l match {
	  case Nil => ""
	  case m::rem => (m.toString)+" "+extMessageListToString(rem)
	}
	
	def transToString=transListToString(validator.getValidTrans)
	def traceToString=
	  extMessageListToString(trace)
	// Pour convertir un ExtMessage en message on doit convertir des Int scala en Nat Isabelle 
	// 
	def extMessage2message(m:ExtMessage):message=
	  m match{
	  	case ExtPay(c,m,tid,am) => if (c>=0 && m>=0 && tid>= 0) Pay((Nat(c),(Nat(m),Nat(tid))),Nat(am)) else throw new IllegalArgumentException("Negative numbers for client, merchant or transaction")
	  	case ExtAck(c,m,tid,am) => if (c>=0 && m>=0 && tid>= 0) Ack((Nat(c),(Nat(m),Nat(tid))),Nat(am)) else throw new IllegalArgumentException("Negative numbers for client, merchant or transaction")
	  	case ExtCancel(c,m,tid) => if (c>=0 && m>=0 && tid>= 0) Cancel((Nat(c),(Nat(m),Nat(tid)))) else throw new IllegalArgumentException("Negative numbers for client, merchant or transaction")
	}
	  
	def traiter(m: ExtMessage){
	  try{
	  	validator.process(extMessage2message(m))
	  	trace= m::trace
	  	update
	  } 
	  	catch {
	  	  case e:IllegalArgumentException => ()
	  	}
	  	
	}
	
	
}