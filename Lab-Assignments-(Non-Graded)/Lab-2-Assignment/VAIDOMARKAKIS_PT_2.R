# In order to execute the code properly, you need to adjust the line below to your
# working directory
setwd(dir="C:\\Users\\User\\Desktop\\Μεταπτυχιακό\\1) Statistics for Business Analytics I\\R Labs\\Graded Assignments")

# Question 1

Drugs <- read.csv(file="Drugs.txt",header=T,sep=",")

head(Drugs)

Drugs <- Drugs[,-6]

head(Drugs)

# Question 2

cat('The number of unique countrie are:',length(unique(Drugs$LOCATION)))

table1 <- sort(table(Drugs$LOCATION), decreasing = FALSE)

View(table1)

# Question 3

quan1 <- quantile(table1)

table2 <- as.matrix(subset(table1,table1 >= quantile(table1, 0.75)))

Drugs2 <- subset(Drugs,Drugs$LOCATION %in% rownames(table2))

View(Drugs2)

# Question 4

library(ggplot2)
library(ggpubr)

PC_HEALTHXP <- ggplot(data=Drugs2, aes(x=TIME, y=PC_HEALTHXP, group =LOCATION, colour =LOCATION)) + geom_line() + ggtitle('Drug spending in PC HEALTHXP') + xlab('Year') + ylab('PC HEALTHXP') + xlim(min(Drugs2$TIME),max(Drugs2$TIME))
PC_GDP <- ggplot(data=Drugs2, aes(x=TIME, y=PC_GDP, group =LOCATION, colour =LOCATION)) + geom_line() + ggtitle('Drug spending in PC GDP') + xlab('Year') + ylab('PC GDP') + xlim(min(Drugs2$TIME),max(Drugs2$TIME))
USD_CAP <- ggplot(data=Drugs2, aes(x=TIME, y=USD_CAP, group =LOCATION, colour =LOCATION)) + geom_line() + ggtitle('Drug spending in USD CAP') + xlab('Year') + ylab('USD CAP') + xlim(min(Drugs2$TIME),max(Drugs2$TIME))
TOTAL_SPEND <- ggplot(data=Drugs2, aes(x=TIME, y=TOTAL_SPEND, group =LOCATION, colour =LOCATION)) + geom_line() + ggtitle('Drug spending in TOTAL SPEND') + xlab('Year') + ylab('TOTAL SPEND') + xlim(min(Drugs2$TIME),max(Drugs2$TIME))

ggarrange(PC_HEALTHXP, PC_GDP, USD_CAP, TOTAL_SPEND, ncol = 2, nrow = 2)

# Question 5

Drugs_GRC <- subset(Drugs,Drugs$LOCATION=='GRC')
Drugs_GRC[order('TIME'),]
rownames(Drugs_GRC) <- 1:nrow(Drugs_GRC)

Drugs_GRC_diff <- within(Drugs_GRC, PC_HEALTHXP_diff <- c(0,diff(Drugs_GRC$PC_HEALTHXP)))
Drugs_GRC_diff <- within(Drugs_GRC_diff, PC_GDP_diff <- c(0,diff(Drugs_GRC$PC_GDP)))
Drugs_GRC_diff <- within(Drugs_GRC_diff, USD_CAP_diff <- c(0,diff(Drugs_GRC$USD_CAP)))
Drugs_GRC_diff <- within(Drugs_GRC_diff, TOTAL_SPEND_diff <- c(0,diff(Drugs_GRC$TOTAL_SPEND)))
all <- nrow(Drugs_GRC_diff)
years <- c(min(Drugs_GRC$TIME),max(Drugs_GRC$TIME))
names(years) <- c('MinYear','MaxYear')

Drugs_GRC_diff$PC_HEALTHXP_pos <- ifelse(Drugs_GRC_diff$PC_HEALTHXP_diff>0,1,0)
Drugs_GRC_diff$PC_GDP_pos <- ifelse(Drugs_GRC_diff$PC_GDP_diff>0,1,0)
Drugs_GRC_diff$USD_CAP_pos <- ifelse(Drugs_GRC_diff$USD_CAP_diff>0,1,0)
Drugs_GRC_diff$TOTAL_SPEND_pos <- ifelse(Drugs_GRC_diff$TOTAL_SPEND_diff>0,1,0)

Drugs_GRC_diff$PC_HEALTHXP_cumsum <- cumsum(Drugs_GRC_diff$PC_HEALTHXP_pos)
Drugs_GRC_diff$PC_GDP_cumsum <- cumsum(Drugs_GRC_diff$PC_GDP_pos)
Drugs_GRC_diff$USD_CAP_cumsum <- cumsum(Drugs_GRC_diff$USD_CAP_pos)
Drugs_GRC_diff$TOTAL_SPEND_cumsum <- cumsum(Drugs_GRC_diff$TOTAL_SPEND_pos)
final_list <- list(Drugs_GRC,years,all)

Drugs_GRC$PC_HEALTHXP_prob <- Drugs_GRC_diff$PC_HEALTHXP_cumsum/ ((1:all) - 1)
Drugs_GRC$PC_GDP_prob <- Drugs_GRC_diff$PC_GDP_cumsum/ ((1:all) - 1)
Drugs_GRC$USD_CAP_prob <- Drugs_GRC_diff$USD_CAP_cumsum/ ((1:all) - 1)
Drugs_GRC$TOTAL_SPEND_prob <- Drugs_GRC_diff$TOTAL_SPEND_cumsum/ ((1:all) - 1)
YearlyProbs <- c(Drugs_GRC$PC_HEALTHXP_prob[all],Drugs_GRC$PC_GDP_prob[all],Drugs_GRC$USD_CAP_prob[all],Drugs_GRC$TOTAL_SPEND_prob[all])
names(YearlyProbs) <- c('PC HEALTHXP','PC GDP','USD CAP','TOTAL SPEND')
final_list <- append(final_list, list(YearlyProbs))

PC_HEALTHXP_binom_prob <- round(dbinom(x=4, 5, prob = Drugs_GRC$PC_HEALTHXP_prob[length(Drugs_GRC$PC_HEALTHXP_prob)],log = FALSE),4)
PC_GDP_binom_prob <- round(dbinom(x=4, 5, prob = Drugs_GRC$PC_GDP_prob[length(Drugs_GRC$PC_GDP_prob)],log = FALSE),4)
USD_CAP_binom_prob <- round(dbinom(x=4, 5, prob = Drugs_GRC$USD_CAP_prob[length(Drugs_GRC$USD_CAP_prob)],log = FALSE),4)
TOTAL_SPEND_binom_prob <- round(dbinom(x=4, 5, prob = Drugs_GRC$TOTAL_SPEND_prob[length(Drugs_GRC$TOTAL_SPEND_prob)],log = FALSE),4)
nofYProbs <- c(PC_HEALTHXP_binom_prob,PC_GDP_binom_prob,USD_CAP_binom_prob,TOTAL_SPEND_binom_prob)
names(nofYProbs) <- c('PC HEALTHXP','PC GDP','USD CAP','TOTAL SPEND')

final_list <- append(final_list, list(nofYProbs))
names(final_list) <- c('Data','Years','Data.points','YearlyProbs','nofYProbs')
head(final_list)

# Question 6

calculate_probability <- function(DATA=NULL, METRIC="pc.gdp", COUNTRY="GRC", nofY=5){
  
  DATA <- as.data.frame(DATA)
  DATA <- subset(DATA, select=c('LOCATION','TIME','PC_HEALTHXP','PC_GDP','USD_CAP','TOTAL_SPEND'))
  DATA <- subset(DATA,DATA$LOCATION==COUNTRY)
  DATA[order('TIME'),]
  rownames(DATA) <- 1:nrow(DATA)
    
  DATA_diff <- within(DATA, PC_HEALTHXP_diff <- c(0,diff(DATA$PC_HEALTHXP)))
  DATA_diff <- within(DATA_diff, PC_GDP_diff <- c(0,diff(DATA$PC_GDP)))
  DATA_diff <- within(DATA_diff, USD_CAP_diff <- c(0,diff(DATA$USD_CAP)))
  DATA_diff <- within(DATA_diff, TOTAL_SPEND_diff <- c(0,diff(DATA$TOTAL_SPEND)))
  tot <- nrow(DATA_diff)
  if (tot-1 <=10){
    return(paste('Unable to calculate probability (n< 10). This dataset has ',(tot-1),' available datapoints for the calculation.',sep=""))
  }
  years <- c(min(DATA$TIME),max(DATA$TIME))
  names(years) <- c('MinYear','MaxYear')
  
  DATA_diff$PC_HEALTHXP_pos <- ifelse(DATA_diff$PC_HEALTHXP_diff>0,1,0)
  DATA_diff$PC_GDP_pos <- ifelse(DATA_diff$PC_GDP_diff>0,1,0)
  DATA_diff$USD_CAP_pos <- ifelse(DATA_diff$USD_CAP_diff>0,1,0)
  DATA_diff$TOTAL_SPEND_pos <- ifelse(DATA_diff$TOTAL_SPEND_diff>0,1,0)
  
  DATA_diff$PC_HEALTHXP_cumsum <- cumsum(DATA_diff$PC_HEALTHXP_pos)
  DATA_diff$PC_GDP_cumsum <- cumsum(DATA_diff$PC_GDP_pos)
  DATA_diff$USD_CAP_cumsum <- cumsum(DATA_diff$USD_CAP_pos)
  DATA_diff$TOTAL_SPEND_cumsum <- cumsum(DATA_diff$TOTAL_SPEND_pos)
  total_list <- list(DATA,years,tot)
  
  DATA$PC_HEALTHXP_prob <- DATA_diff$PC_HEALTHXP_cumsum/ ((1:tot) - 1)
  DATA$PC_GDP_prob <- DATA_diff$PC_GDP_cumsum/ ((1:tot) - 1)
  DATA$USD_CAP_prob <- DATA_diff$USD_CAP_cumsum/ ((1:tot) - 1)
  DATA$TOTAL_SPEND_prob <- DATA_diff$TOTAL_SPEND_cumsum/ ((1:tot) - 1)
  YearlyProbs <- c(DATA$PC_HEALTHXP_prob[tot],DATA$PC_GDP_prob[tot],DATA$USD_CAP_prob[tot],DATA$TOTAL_SPEND_prob[tot])
  names(YearlyProbs) <- c('PC HEALTHXP','PC GDP','USD CAP','TOTAL SPEND')
  total_list <- append(total_list, list(YearlyProbs))
  
  PC_HEALTHXP_binom_prob <- round(dbinom(x=(nofY-1), nofY, prob = DATA$PC_HEALTHXP_prob[length(DATA$PC_HEALTHXP_prob)],log = FALSE),4)
  PC_GDP_binom_prob <- round(dbinom(x=(nofY-1), nofY, prob = DATA$PC_GDP_prob[length(DATA$PC_GDP_prob)],log = FALSE),4)
  USD_CAP_binom_prob <- round(dbinom(x=(nofY-1), nofY, prob = DATA$USD_CAP_prob[length(DATA$USD_CAP_prob)],log = FALSE),4)
  TOTAL_SPEND_binom_prob <- round(dbinom(x=(nofY-1), nofY, prob = DATA$TOTAL_SPEND_prob[length(DATA$TOTAL_SPEND_prob)],log = FALSE),4)
  nofYProbs <- c(PC_HEALTHXP_binom_prob,PC_GDP_binom_prob,USD_CAP_binom_prob,TOTAL_SPEND_binom_prob)
  names(nofYProbs) <- c('pc.tot','pc.gdp','pc.ca','total')
  
  total_list <- append(total_list, list(nofYProbs))
  names(total_list) <- c('Data','Years','Data.points','YearlyProbs','nofYProbs')
  return(paste('Based on ',tot,' datapoints from years ',total_list$Years[1],' to ',total_list$Years[2],', the probability that ',COUNTRY,' will increase its drug expenditure, in terms of ',METRIC,', in at least ',(nofY-1),' years in the period ',(total_list$Years[2]+1),' to ',(total_list$Years[2]+1+nofY),' is ',nofYProbs[METRIC],sep=""))
}

# Below, you will find 2 examples in order to test the function:

#calculate_probability(Drugs, 'pc.tot', 'RUS', 5)
#calculate_probability(Drugs, 'pc.tot', 'GRC', 5)
