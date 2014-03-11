package com.kyub.scala.bank

import java.util.Date


trait AccountOperation{
  
	def refDate():Date
	
	def updateBalance(): Double
  
}

trait TransactionContainer{
  
  private var transactions = List[AccountOperation]()
  
  def addTransaction(op:AccountOperation) =  transactions ::= op
  
  def getTransactions():List[AccountOperation] = transactions
  
}

trait Asset{
  
  def value():Double
  
}


class Deposit(depositMoney: Double, depositDate: Date) extends AccountOperation{
	
	def refDate() = {
	  depositDate
	}  
	
	def updateBalance() = {
	  depositMoney
	}
  
}

class Withdraw(depositMoney: Double, depositDate: Date) extends AccountOperation{
	
	def refDate() = {
	  depositDate
	}  
	
	def updateBalance() = {
	  depositMoney * (-1.0)
	}
  
}


class BankAccount(val initialBalance: Double = 0) extends TransactionContainer with Asset {
  
  def value() = {
    val extractedLocalValue = getTransactions() 
    extractedLocalValue.size match {
      case  x if x > 0 => initialBalance + (extractedLocalValue map { (_.updateBalance())} ). reduceLeft { (_ + _) }
      case otherNumber => initialBalance
    }
    
  } 

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

class CreditCard(val maxCover: Double)  extends TransactionContainer{
  
  def expenseSoFar() ={ 
    val extractedLocalValue = getTransactions() 
    extractedLocalValue.size match {
      case  x if x > 0 => (extractedLocalValue map { (_.updateBalance())} ). reduceLeft { (_ + _) }
      case otherNumber => 0.
    }
  } 
  
  
  def withdraw(money: Double) {
    money match {
      case x if x < 0 => println("Failing silenty")
      case x if (x + expenseSoFar()) > maxCover => println("Refused " + x + " + " +  expenseSoFar() + " > " + maxCover )
      case otherNumber =>   addTransaction(new Withdraw(money,new Date()))
    }
  }
  
  
}