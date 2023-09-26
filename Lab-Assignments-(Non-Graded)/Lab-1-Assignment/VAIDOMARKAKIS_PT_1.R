# Question 1

library(robotstxt)
library(rvest)
# This code takes from metacritic.com all the publications from page 0 to page 27
# of The Washington Post and it creates a data frame with observations with columns title,
# metascore, critic and date posted
# Check whether scraping is allowed from this webpage (returns TRUE)
# ATTENTION: PUT THE WHOLE URL IN ONE LINE WHEN RUNNING THE CODE
paths_allowed("https://www.metacritic.com/publication/washington-post?filter=movies&num_items=100&sort_options=date&page=0")
# Define character element "main.page", to be used recursively for defining
# multiple pages from metacritic.com
# ATTENTION: PUT THE WHOLE URL IN ONE LINE WHEN RUNNING THE CODE
main.page <- "https://www.metacritic.com/publication/washington-post?filter=movies&num_items=100&sort_options=date&page="
for (i in 0:27){ # This is a "for" loop.
  # This means that all the lines until the closure of }
  # will be repeated for different values of object i
  # thus, on the first run i=0, second run i=1,... last run i=27
  # for each step, define...
  step.page <- paste(main.page,i,sep="") # it completes the link from the main.page with a page number from 0 to 27
  webdata <-read_html(step.page) # OK
  # Vector ... is created which includes .....
  title <-c(webdata %>% html_nodes("div.review_product") %>% html_nodes("a") %>%
              html_text()) #Vector title is created which includes movie titles
  metascore <- c(webdata %>% html_nodes("li.review_product_score.brief_metascore") %>%
                   html_nodes("span.metascore_w") %>% html_text()) #Vector metascore is created which includes public reviews
  critic <- c(webdata %>% html_nodes("li.review_product_score.brief_critscore") %>%
                html_nodes("span.metascore_w") %>% html_text()) #Vector critic is created which includes critics reviews
  date <- c(webdata %>% html_nodes("li.review_action.post_date") %>% html_text()) #Vector date is created which includes post date
  if (length(date)<100 ){for (j in length(date):100){ date[j] <- date[length(date)]}} #OK
  a <- substr(date,12,13) #Vector a is created which includes the date of the month post released
  b <- substr(date,8,10) #Vector b is created which includes the month post released
  d <- substr(date,16,19) #Vector c is created which includes the year post released
  lct <- Sys.getlocale("LC_TIME"); Sys.setlocale("LC_TIME", "C") #OK
  date2 <- apply(cbind(a,b,d),1,paste,collapse="/") #Vector date2 is created which includes the date in the format of Day/Month/year
  date3 <- as.Date(date2,"%d/%b/%Y") #Vector date3 is created which includes the date2 in the format of yyyy-mm-dd
  Sys.setlocale("LC_TIME", lct) #OK
  df = data.frame(title,metascore,critic,date3) #it creates a data frame with title(chr), metascore(chr), critic(chr) and date(Date)
  colnames(df) <- c("title", "metascore", "critic","date") #it defines the column names in the data frame df
  df$metascore <- as.numeric(as.character(df$metascore)) #convert the metascore from (chr) to (int)
  df$critic <- as.numeric(as.character(df$critic)) #?convert the critic from (chr) to (int)
  df <- df[complete.cases(df), ] #it removes rows with missing values in any column of df
  if (i==0){ #OK
    df.tot <- df} #OK
  if (i>0){ #from second data frame and above...
    df.tot <- rbind(df.tot,df) } #it is used to bind the multiple dfs to the df.tot each time the loop is executed
}
df.tot$title <- as.character(df.tot$title) #convert the titles to (chr) at df.tot

# Question 2

str(df.tot) #2774 obs.
### df.tot is the data frame with 4 columns [title(chr), metascore(num), critic(num) and date(Date)]
### from The Washington Post 

# Question 3

df.tot$ratio <- df.tot$metascore/df.tot$critic
perc_function <- function(x) trunc(rank(x))/length(x)
df.tot$perc.meta <- perc_function(df.tot$metascore)
df.tot$perc.critic <- perc_function(df.tot$critic)
df.tot$year <- format(df.tot$date, format="%Y")

# Question 4

df.tot$title[which(df.tot$metascore==max(df.tot$metascore))]

# Question 5

boxplot(perc.meta ~ year, data = df.tot, xlab = "Year",
        ylab = "Percentile of metascore", main = "Percentile of metascore per Year")
abline(h = 0.5, col="red", lwd=3, lty=2)

# As we observe from the plot, in the year 2013, we have the lowest 
# metascore in comparison with all the metascore from all years and 
# in 2020, we have the highest scores in comparison with all the metascore
# from all years. The above show to us that maybe, in 2013, they came out movies
# that the public didn't like and put a low score. On the other hand, in 2020,
# they came out movies that the public liked and put a high score. In general,
# as the years go by, better movies produced for the public. We can see that
# the all the boxes have similar shape with the median of every year to be almost
# in the middle of the box, which declares that the opinion of the whole public
# is rising.

# Question 6

# In some cases, ratio is Inf because the critic score was 0 and 0 cannot be
# a denominator in a fraction. If 0 is a denominator in a fraction,
# we use lim to define the result which is defined from the sign of the numerator
# of the fraction. The result is either Inf or -Inf.

df.tot2<-df.tot[!is.infinite(df.tot$ratio),] #2764 obs. compare to 2774 obs. of df.tot

# Question 7

matrix.tot3 <- data.matrix(cbind(df.tot2$metascore,df.tot2$critic))

colnames(matrix.tot3) <- c('metascore','critic')

head(matrix.tot3)

vector.tot4 <- apply(matrix.tot3,1,mean)

head(vector.tot4)

# Question 8

plot(df.tot2$date,df.tot2$perc.meta, xlab = "Year",
        ylab = "Percentile of metascore", main = "Metascores percentiles",
     col = ifelse(df.tot2$metascore>50,'blue','red'),las=1)
abline(h = max(df.tot2$perc.meta[df.tot2$metascore==50]), col="green", lwd=3, lty=2)

# Question 9

# From all the public critics that we have, 79% (78,76712% to be more precise) of 
# them have a metascore over 50 while the remaining 21% (21,23288% to be more precise)
# of them have a metascore lower than 50. In general, as we see from the scatter plot, people
# tend to vote higher than the middle of the possible range in order to evaluate a movie.
