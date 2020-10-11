###############
# directories #
###############
getwd()
setwd("C:/Dhruv/Misc/Personal/writing/Blogging/2_posts/3_September/wk2_post9/2_Post 9_analysis")

########
# data #
########
sec <- read.csv("final_SEC_data.csv")

########
# eda #
########

# vars of interest, representing 4 sections

# AccountsPayableCurrent - 1	
# AccumulatedOtherComprehensiveIncomeLossNetOfTax - 2
# PropertyPlantAndEquipmentNet - 3 	
# CommonStockParOrStatedValuePerShare - 4

summary(sec)

# number of na's ranges from 3219/6028 to 5521/6027
# not all companies will have entries for every column on a financial statement
# eg companies that don't own heavy machinery will report rented gear in cash flows.

# (i)
hist(subset(sec, AccountsPayableCurrent < 1.342e+08 & AccountsPayableCurrent > 1.376e+06)$AccountsPayableCurrent, 
     main = "Accounts Payable")

# (ii)
hist(subset(sec, AccumulatedOtherComprehensiveIncomeLossNetOfTax < 2.931e+04 & AccumulatedOtherComprehensiveIncomeLossNetOfTax > -4.915e+07)$AccumulatedOtherComprehensiveIncomeLossNetOfTax, 
     main = "Bottom Line")

# (iii)
hist(subset(sec, PropertyPlantAndEquipmentNet < 2.289e+08 & PropertyPlantAndEquipmentNet > 1.092e+06)$PropertyPlantAndEquipmentNet, 
     main = "Property, Plant, and Equipment")

# (iv)
hist(subset(sec, CommonStockParOrStatedValuePerShare < 0.05 & CommonStockParOrStatedValuePerShare > 0)$CommonStockParOrStatedValuePerShare, 
     main = "Stock Pricing")



#############
# cutpoints #
#############


# (i) balance sheet

quantile(sec$AccountsPayableCurrent, c(.33, .66), na.rm = TRUE) 

sec$i_cutpoint = ""

for (i in 1:nrow(sec)){
  if (!is.na(sec$AccountsPayableCurrent[i]) & (sec$AccountsPayableCurrent[i]<=2798857)){
    sec$i_cutpoint[i] <- "low"
  } else if (!is.na(sec$AccountsPayableCurrent[i]) & (sec$AccountsPayableCurrent[i]>68671700)){
    sec$i_cutpoint[i] <- "high"
  } else if ((!is.na(sec$AccountsPayableCurrent[i])) & (sec$AccountsPayableCurrent[i]>2798857) & (sec$AccountsPayableCurrent[i])<=68671700){
    sec$i_cutpoint[i] <- "mid"
  }
}

table(sec$i_cutpoint)

# (ii) income statement

quantile(sec$AccumulatedOtherComprehensiveIncomeLossNetOfTax, c(.33, .66), na.rm = TRUE) 

sec$ii_cutpoint = ""

for (i in 1:nrow(sec)){
  if (!is.na(sec$AccumulatedOtherComprehensiveIncomeLossNetOfTax[i]) & (sec$AccumulatedOtherComprehensiveIncomeLossNetOfTax[i]<=(-23993170.0))){
    sec$ii_cutpoint[i] <- "low"
  } else if (!is.na(sec$AccumulatedOtherComprehensiveIncomeLossNetOfTax[i]) & (sec$AccumulatedOtherComprehensiveIncomeLossNetOfTax[i]>(-148086.3))){
    sec$ii_cutpoint[i] <- "high"
  } else if ((!is.na(sec$AccumulatedOtherComprehensiveIncomeLossNetOfTax[i])) & (sec$AccumulatedOtherComprehensiveIncomeLossNetOfTax[i]>(-23993170.0)) & (sec$AccumulatedOtherComprehensiveIncomeLossNetOfTax[i])<=(-148086.3)){
    sec$ii_cutpoint[i] <- "mid"
  }
}

table(sec$ii_cutpoint)


# (iii) operation expenses

quantile(sec$PropertyPlantAndEquipmentNet , c(.33, .66), na.rm = TRUE) 

sec$iii_cutpoint = ""

for (i in 1:nrow(sec)){
  if (!is.na(sec$PropertyPlantAndEquipmentNet [i]) & (sec$PropertyPlantAndEquipmentNet [i]<=5220200)){
    sec$iii_cutpoint[i] <- "low"
  } else if (!is.na(sec$PropertyPlantAndEquipmentNet [i]) & (sec$PropertyPlantAndEquipmentNet [i]>124741860)){
    sec$iii_cutpoint[i] <- "high"
  } else if ((!is.na(sec$PropertyPlantAndEquipmentNet [i])) & (sec$PropertyPlantAndEquipmentNet [i]>5220200) & (sec$PropertyPlantAndEquipmentNet [i])<=124741860){
    sec$iii_cutpoint[i] <- "mid"
  }
}

table(sec$iii_cutpoint)

# (iv) shareholders equity

quantile(sec$CommonStockParOrStatedValuePerShare  , c(.33, .66), na.rm = TRUE) 

sec$iv_cutpoint = ""

for (i in 1:nrow(sec)){
  if (!is.na(sec$CommonStockParOrStatedValuePerShare  [i]) & (sec$CommonStockParOrStatedValuePerShare  [i]<=0.001)){
    sec$iv_cutpoint[i] <- "low"
  } else if (!is.na(sec$CommonStockParOrStatedValuePerShare  [i]) & (sec$CommonStockParOrStatedValuePerShare  [i]>0.010)){
    sec$iv_cutpoint[i] <- "high"
  } else if ((!is.na(sec$CommonStockParOrStatedValuePerShare  [i])) & (sec$CommonStockParOrStatedValuePerShare  [i]>0.001) & (sec$CommonStockParOrStatedValuePerShare  [i])<=0.010){
    sec$iv_cutpoint[i] <- "mid"
  }
}

table(sec$iv_cutpoint)


###################################
# regression models: introduction #
###################################

# (i) balance sheet
fit <- lm(AccumulatedOtherComprehensiveIncomeLossNetOfTax ~ AccountsPayableCurrent + 
            AccountsReceivableNetCurrent, sec)
fit

summary(fit)

# (ii) income statement
fit <- lm(AccumulatedOtherComprehensiveIncomeLossNetOfTax ~ ComprehensiveIncomeNetOfTax + 
            NetIncomeLoss + 
            OperatingIncomeLoss + 
            RetainedEarningsAccumulatedDeficit, sec)
fit

summary(fit)


# (iii) operating expenses
fit <- lm(AccumulatedOtherComprehensiveIncomeLossNetOfTax ~ CashAndCashEquivalentsAtCarryingValue + 
            DepreciationAndAmortization + 
            DepreciationDepletionAndAmortization + 
            PropertyPlantAndEquipmentNet, sec)
fit

summary(fit)


# (iv) shareholders equity
fit <- lm(AccumulatedOtherComprehensiveIncomeLossNetOfTax ~ CommonStockParOrStatedValuePerShare + 
            CommonStockSharesAuthorized + 
            CommonStockSharesIssued + 
            CommonStockSharesOutstanding + 
            WeightedAverageNumberOfSharesOutstandingBasic, sec)
fit

summary(fit)


# Variables List
#
# AccountsPayableCurrent - 1	
# AccountsReceivableNetCurrent - 1
# 
# AccumulatedOtherComprehensiveIncomeLossNetOfTax - 2
# ComprehensiveIncomeNetOfTax - 2
# NetIncomeLoss - 2
# OperatingIncomeLoss - 2	
# RetainedEarningsAccumulatedDeficit - 2	
# 
# CashAndCashEquivalentsAtCarryingValue - 3
# DepreciationAndAmortization - 3
# DepreciationDepletionAndAmortization - 3	
# PropertyPlantAndEquipmentNet - 3 	
# 
# CommonStockParOrStatedValuePerShare - 4
# CommonStockSharesAuthorized - 4
# CommonStockSharesIssued - 4
# CommonStockSharesOutstanding - 4	
# WeightedAverageNumberOfSharesOutstandingBasic - 4


