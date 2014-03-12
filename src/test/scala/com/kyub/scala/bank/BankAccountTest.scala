package com.kyub.scala.bank

import org.scalatest._
import java.util.Date

class BankAccountTest extends FlatSpec with Matchers {
	
	"A Bank account " should " have a default balance of Zero" in {
	  val bankAccount = new BankAccount()
	  bankAccount.value() should be (0.)
	}
	
	it should "heve an initial balance if provided" in {
	  val bankAccount = new BankAccount(10.)
	  bankAccount.value() should be (10.)
	}
	
	it should "allow deposit of positive ammount of money" in {
	   val bankAccount = new BankAccount()
	   bankAccount.deposit(10.)
	   bankAccount.value() should be (10.)
	   bankAccount.deposit(-10.)
	   bankAccount.value() should be (10.)
	   
	}
	
	it should "allow withdraw of positive ammount of money" in {
	   val bankAccount = new BankAccount(10.)
	   bankAccount.withdraw(5.)
	   bankAccount.value() should be (5.)
	   bankAccount.withdraw(-10.)
	   bankAccount.value() should be (5.)
	  
	}
	
	it should "not allow withdraw of more money then deposit" in {
	   val bankAccount = new BankAccount(1.)
	   bankAccount.withdraw(5.)
	   bankAccount.value() should be (0.)
	  
	}
	
	it should " track all transactions " in {
	  val bankAccount = new BankAccount()
	   bankAccount.deposit(10.)
	   bankAccount.withdraw(5.)
	   bankAccount.withdraw(9.)
	   bankAccount.value() should be (0.)
	   bankAccount.getTransactions().size should be (3)
	  
	}
	
	it should " be an asset with a value" in {
	  val bankAccount = new BankAccount(10.)
	  bankAccount.value should be (10.)
	  
	}	
	
	"A credit card" should " track all transactions " in {
	  val creditCard = new CreditCard(100.)

	   creditCard.withdraw(5.)
	 
	   creditCard.withdraw(9.)

	   creditCard.withdraw(9.)
	 
	   creditCard.getTransactions().size should be (3)
	  
	}
	
	it should "refuse withdraw over max cover" in {
	   val creditCard = new CreditCard(100.)
	   creditCard.withdraw(5.)
	   creditCard.withdraw(96.)
	   creditCard.getTransactions().size should be (1)
	  
	}
	
	"A Risk Manager" should "accept Risky Operations" in {
	 
	  RiskManager.addRiskyOperation(new Withdraw(10.,new Date()), new BankAccount()) should be (true)
	  val creditCard = new CreditCard(5)
	  (RiskManager.addRiskyOperation(new Withdraw(10.,new Date()), creditCard)) should be (true)
	  
	  
	}
	
	it should " manage risk grade" in {
	  
	  RiskManager.riskGrade should be (2.)
	  
	}
	
}