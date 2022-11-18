{-# LANGUAGE OverloadedStrings #-}

module Main where

import           DDD
import           RiskManager
import           RiskManager.Types

myUserData :: UserData
myUserData = UserData
  { name = "Marco"
  , surname = "Perone"
  , taxCode = "PRNMRC83S14C957V"
  }

myLoanDetails :: LoanDetails
myLoanDetails = LoanDetails
  { amount      = EuroCents 10000
  , instalments = InstalmentsNumber 12
  }

main :: IO ()
main =
  -- run the whole computation in the IO effect
  print =<<
    runApplication
      riskManagerApplication -- machines combine into one
      [ RegisterUserData myUserData
      , RegisterUserData myUserData
      , ProvideLoanDetails myLoanDetails
      , RegisterUserData myUserData
      ] -- commands

  -- prints (original):
  -- ReceivedData
  --  {userData = Just (UserData {name = Name "Marco", surname = Surname "Perone", taxCode = TaxCode "PRNMRC83S14C957V"})
  --  , loanDetails = Just (LoanDetails {amount = EuroCents 10000, instalments = InstalmentsNumber 12})
  --  , creditBureauData = Just (CreditBureauData {missedPaymentDeadlines = MissedPaymentDeadlines 2, arrears = EuroCents 6})}

  -- prints (different):
  -- UserDataUpdatesCount 3

  -- prints (combined):
  -- (ReceivedData
  --  {userData = Just (UserData {name = Name "Marco", surname = Surname "Perone", taxCode = TaxCode "PRNMRC83S14C957V"})
  --  , loanDetails = Just (LoanDetails {amount = EuroCents 10000, instalments = InstalmentsNumber 12})
  --  , creditBureauData = Just (CreditBureauData {missedPaymentDeadlines = MissedPaymentDeadlines 2, arrears = EuroCents 6})}
  -- , UserDataUpdatesCount 3)
