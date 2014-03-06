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


class BankAccount(var balance: Double = 0) extends TransactionContainer {

  def deposit(money: Double) {
    if (money > 0.)
      balance = balance + money;
      addTransaction(new Deposit(money,new Date()))
  }

  def withdraw(money: Double) {
    money match {
      case x if x < 0 => println("Failing silenty")
      case x if x > balance => balance = 0. ; addTransaction(new Withdraw(x-balance,new Date()))
      case otherNumber => balance = balance - money;  addTransaction(new Withdraw(money,new Date()))
    }

  }

}