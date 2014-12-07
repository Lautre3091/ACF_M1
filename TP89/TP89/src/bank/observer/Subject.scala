package bank.observer

trait Subject {
	def addObserver(o:Observer)
	def removeObserver(o:Observer)
	def update
}