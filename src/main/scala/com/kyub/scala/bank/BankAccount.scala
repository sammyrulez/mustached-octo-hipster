package com.kyub.scala.bank

class BankAccount(var balance: Double = 0) {

  def deposit(money: Double) {
    if (money > 0.)
      balance = balance + money
  }

  def withdraw(money: Double) {
    money match {
      case x if x < 0 => println("Failing silenty")
      case x if x > balance => balance = 0.
      case otherNumber => balance = balance - money
    }

  }

}