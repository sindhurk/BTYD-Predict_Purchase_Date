library(BTYDplus)
library(BTYD)
library(dplyr)
library(data.table)

# Set working directory and read data as df
setwd("C:/Users/hp/Desktop/Teamstreamz")
df=read.csv("explore.csv")

# create empty dataframe to subset required data
ts_data<-data.frame()

# subset Customer.Unique.ID, Invoice.Date, Sales.Volume.Pack and rename columns
ts_data<-select(df, Customer.Unique.ID, Invoice.Date, Sales.Volume.Pack)
setnames(ts_data, old = c('Customer.Unique.ID','Invoice.Date','Sales.Volume.Pack'), new = c('cust','date','sales'))

# merge purchase on same date by one customer as 1 transaction ---- I am not considering quantity purchased,
# as I am using only recency, frequency and the duration between each transaction
ts_data<-dc.MergeTransactionsOnSameDate(ts_data)

# split data upto 2016-06-30 for training
ts_data$date<-as.Date(ts_data$date)
end.of.cal.period <- as.Date("2016-06-30")
ts_data.cal <- ts_data[which(ts_data$date <= end.of.cal.period), ]
split.data <- dc.SplitUpElogForRepeatTrans(ts_data.cal)
clean.elog <- split.data$repeat.trans.elog

# create table with date vs customer
freq.cbt <- dc.CreateFreqCBT(clean.elog)
# view the above table 
freq.cbt[1:5,1:8]

# above table has customers with more than 2 transaction - to compute period between each transaction
tot.cbt <- dc.CreateFreqCBT(ts_data)
# combine customers with 1 transaction to above table - they are also important 
cal.cbt <- dc.MergeCustomers(tot.cbt, freq.cbt)

# transform above table to customer by sufficient statistic table
birth.periods <- split.data$cust.data$birth.per
last.dates <- split.data$cust.data$last.date
cal.cbs.dates <- data.frame(birth.periods, last.dates,
                            end.of.cal.period)
cal.cbs <- dc.BuildCBSFromCBTAndDates(cal.cbt, cal.cbs.dates,     # to compute frequency of transaction for
                                      per="month")               #  month (change per = 'week' for weekly computation)


# Compute parameters -- assuming the data takes a negative binomial distribution 
params <- bgnbd.EstimateParameters(cal.cbs)
params
LL <- bgnbd.cbs.LL(params, cal.cbs)
LL

# try more epochs until convergence
p.matrix <- c(params, LL);
for (i in 1:5){
  params <- bgnbd.EstimateParameters(cal.cbs, params);
  LL <- bgnbd.cbs.LL(params, cal.cbs);
  p.matrix.row <- c(params, LL);
  p.matrix <- rbind(p.matrix, p.matrix.row);
}
colnames(p.matrix) <- c("r", "alpha", "a", "b", "LL");
rownames(p.matrix) <- 1:6;
p.matrix;


# select customer to view frequency for next month
cal.cbs["SG_001|000|300/C051",]

# compute customer to view frequency for next month
x <- cal.cbs["SG_001|000|300/C051", "x"]
t.x <- cal.cbs["SG_001|000|300/C051", "t.x"]
T.cal <- cal.cbs["SG_001|000|300/C051", "T.cal"]
bgnbd.ConditionalExpectedTransactions(params, T.star = 1,     # T.star=1 computes frequency for next 1 month
                                      x, t.x, T.cal)          # change T.star value to chk freq for next n months


# If frequency is less than 0.5, assume there was no trnnsaction for the upcomimg month
#
