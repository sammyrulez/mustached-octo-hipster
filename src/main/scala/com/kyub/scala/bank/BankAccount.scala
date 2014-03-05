package com.kyub.scala.bank

class BankAccount(var balance: Double = 0) {
  
  def deposit(money:Double){
    if(money > 0.)
    	balance = balance + money
  }
  
  def withdraw(money:Double){
    if(money > 0.)
    	if(money > balance)
    		balance = 0.
    	else
    		balance = balance - money
  }

}