# parent directory
getwd()
setwd("C:/Dhruv/Misc/Personal/writing/Blogging/2_posts/3_September/wk2_post9/1_Post 9_blueprint/SEC Financial Statements Data")

# Null Dataset
df <- data.frame(matrix(ncol = 7, nrow = 0))

# Macro looping in All datasets
for (year in 2009:2020){
  for (qtr in 1:4){
    setwd(paste0("C:/Dhruv/Misc/Personal/writing/Blogging/2_posts/3_September/wk2_post9/1_Post 9_blueprint/SEC Financial Statements Data/", as.character(year), "q", as.character(qtr)))
    num <- read.table(
      "num.txt",
      sep="\t", fill=TRUE, header=TRUE)
    
    sub <- read.table(
      "sub.txt",
      sep="\t", fill=TRUE, header=TRUE)
    
    # merging sub and num
    numsub <- merge(num, sub, by="adsh")
    
    # keeping relevant columns
    numsub <- numsub[c("name", "tag", "value", "countryba", "stprba", "fp","fy")]
    
    # subsetting to US companies only
    numsub <- subset(numsub, numsub$countryba == "US")
    # keeping relevant financial statement variables
    numsub <- subset(numsub, numsub$tag == "ComprehensiveIncomeNetOfTax" | 
                       numsub$tag == "RetainedEarningsAccumulatedDeficit" |
                       numsub$tag == "WeightedAverageNumberOfSharesOutstandingBasic" |
                       numsub$tag == "PropertyPlantAndEquipmentNet" |
                       numsub$tag == "CommonStockSharesIssued" |
                       numsub$tag == "CommonStockSharesAuthorized" |
                       numsub$tag == "AccumulatedOtherComprehensiveIncomeLossNetOfTax" |
                       numsub$tag == "DepreciationDepletionAndAmortization" |
                       numsub$tag == "CommonStockSharesOutstanding" |
                       numsub$tag == "AccountsReceivableNetCurrent" |
                       numsub$tag == "AccountsPayableCurrent" |
                       numsub$tag == "CommonStockParOrStatedValuePerShare" |
                       numsub$tag == "OperatingIncomeLoss" |
                       numsub$tag == "DepreciationAndAmortization" |
                       numsub$tag == "CashAndCashEquivalentsAtCarryingValue" |
                       numsub$tag == "NetIncomeLoss")
    
    # restricting to current fy+fp
    numsub <- subset(numsub, numsub$fy == as.character(year))
    numsub <- subset(numsub, numsub$fp == paste0("Q", qtr))
    
    # appending
    df = rbind(df, numsub)
    
    # printing progress
    print(paste0(as.character(year),"Q", qtr))
  } 
}
rm(numsub, num, sub)

# converting factors to characters
df$name <- as.character(df$name)
df$fp <- as.character(df$fp)
df$tag <- as.character(df$tag)

# tidying output dataset
df2 <- subset(df, df$fp == "Q1" | df$fp == "Q2" | df$fp == "Q3" | df$fp == "Q4")

# aggregating duplicate rows to obtain mean value
df2 <- aggregate(value~name+tag+fp+fy, df2, mean)

# reordering columns
df2 <- df2[c("name", "fy", "fp", "tag", "value")]

# ordering by name
df2 <- df2[order(df2$name, df2$fy, df2$fp),]
rownames(df2) <- NULL

# furling out
library(tidyr)
df_wide <- spread(df2, tag, value, fill = NA, convert = FALSE,
                  drop = TRUE, sep = NULL)

# ordering by name, fy, fp
df_wide <- df_wide[order(df_wide$name, df_wide$fy, df_wide$fp),]

# resetting index
rownames(df_wide) <- NULL

# writing out csv
write.csv(df_wide, "C:/Dhruv/Misc/Personal/writing/Blogging/2_posts/3_September/wk2_post9/1_Post 9_blueprint/final_SEC_data.csv")
