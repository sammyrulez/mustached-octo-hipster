package com.kyub.scala.bank

import java.util.Date


trait AccountOperation{
  
	def refDate :Date
	
	def updateBalance(): Double
  
}

object RiskManager{
  
	var riskGrades = List(0)
	
	def riskGrade: Int = (riskGrades reduceLeft { (_ + _) })
	
	
	def addRiskyOperation(op:AccountOperation,accountable:Any):Boolean = {
	  
	  accountable match {
	  	case b : BankAccount => if (b.value() + op.updateBalance() < 0) {riskGrades =  1 :: riskGrades} ; return true
	  	case c : CreditCard =>  if (c.computedActualValue + op.updateBalance() < (-1.0)*c.maxCover) {riskGrades =  1 :: riskGrades} ; return true
	  	case other => println("Unsupported"); return false
	  }
	}  
}



trait TransactionContainer{
  
  private var transactions = List[AccountOperation]()
  
  def addTransaction(op:AccountOperation) =  transactions ::= op
  
  def getTransactions():List[AccountOperation] = transactions
  
  def computedActualValue: Double = {    
    transactions.size match {
      case  x if x > 0 => (transactions map { (_.updateBalance())} ). reduceLeft { (_ + _) }
      case otherNumber => 0.
    }
  }
  
  
}

trait Asset{
  
  def value :Double
  
}


class Deposit(depositMoney: Double, depositDate: Date) extends AccountOperation{
	
	def refDate() = {
	  depositDate
	}  
	
	def updateBalance() = {
	  depositMoney
	}
  
}

class Withdraw(withdrawMoney: Double, depositDate: Date) extends AccountOperation{
	
	def refDate =  depositDate
	
	
	def updateBalance() = {
	  withdrawMoney * (-1.0)
	}
  
}


class BankAccount(val initialBalance: Double = 0) extends TransactionContainer with Asset {
  
  def value() = initialBalance + computedActualValue
    
  def deposit(money: Double) {
    if (money > 0.)      
      addTransaction(new Deposit(money,new Date()))
  }

  def withdraw(money: Double) {
    money match {
      case x if x < 0 => println("Failing silenty")
      case x if x > value() => addTransaction(new Withdraw(value(),new Date()))
      case otherNumber =>  addTransaction(new Withdraw(money,new Date()))
    }

  }

}

class CreditCard(val maxCover: Double)  extends TransactionContainer {
  
 
  
  
  def withdraw(money: Double) {
    
    
    money match {
      case x if x < 0 => println("Failing silenty")
      case x if (x + ( (-1.0) * computedActualValue)) > maxCover => println("Refused " + x + " + " + computedActualValue + " > " + maxCover )
      case otherNumber =>   addTransaction(new Withdraw(money,new Date()))
    }
  }
  
 
  
  
}