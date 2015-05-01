package frdomain.ch6
package domain
package service

trait InterestPostingService[Account, Amount] 
  extends InterestCalculation[Account, Amount]
  with TaxCalculation[Amount]

