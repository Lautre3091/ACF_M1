package bank

/* The Scala Trait to implement */ 
trait TransValidator {
	def process(m: message)
	def getValidTrans: List[((Nat, (Nat, Nat)),Nat)]
	def authors: String
}