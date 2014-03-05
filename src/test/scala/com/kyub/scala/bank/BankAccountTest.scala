package com.kyub.scala.bank

import org.scalatest._

class BankAccountTest extends FlatSpec with Matchers {
	
	"A Bank account " should " have a default balance of Zero" in {
	  val bankAccount = new BankAccount()
	  bankAccount.balance should be (0.)
	}
	
	it should "heve an initial balance if provided" in {
	  val bankAccount = new BankAccount(10.)
	  bankAccount.balance should be (10.)
	}
	
	it should "allow deposit of positive ammount of money" in {
	   val bankAccount = new BankAccount()
	   bankAccount.deposit(10.)
	   bankAccount.balance should be (10.)
	   bankAccount.deposit(-10.)
	   bankAccount.balance should be (10.)
	   
	}
	
	it should "allow withdraw of positive ammount of money" in {
	   val bankAccount = new BankAccount(10.)
	   bankAccount.withdraw(5.)
	   bankAccount.balance should be (5.)
	   bankAccount.withdraw(-10.)
	   bankAccount.balance should be (5.)
	  
	}
	
	it should "not allow withdraw of more money then deposit" in {
	   val bankAccount = new BankAccount(1.)
	   bankAccount.withdraw(5.)
	   bankAccount.balance should be (0.)
	  
	}
	
	
	
	
}