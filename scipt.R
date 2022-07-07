#Load data
df <- rio::import("data/Data_Survey.csv")
View(df)
names(df)
df[, c(3,4,5,7)] <- NULL
df <- dplyr::rename(df, fsale_perc = Q27_1, ofsale_perc = Q28_1)
df <- dplyr::rename(df, comp1 = Q10_1, comp2 = Q10_2, comp3 = Q10_3, comp4 = Q10_4)
df <- dplyr::rename(df, company = Company, id = ID, age = Age)
df <- dplyr::rename(df, channel = Channel)
df <- dplyr::rename(df, partner = Partner)
names(df)[names(df) %in% c("Scope of internationalization","International experience")] <- c("scope","int_exp")
names(df)[names(df) %in% c("Firm size","Foreign sales","Own in-store","Own online","Channel complexity")] <- c("size","fsales?","own_store","own_onl","channel_complex")
names(df)[names(df) %in% c("Total foreign sales range","Total online sales range")] <- c("fsale_range","osale_range")
names(df)[names(df) %in% c("Q22_1","Q22_2","Q22_3","Q22_4","Q22_5")] <- c("mdriven1","mdriven2","mdriven3","mdriven4","mdriven5")
names(df)[names(df) %in% c("Q19_1","Q19_2","Q19_3","Q19_4","Q19_5","Q19_6")] <- c("onl_cap1","onl_cap2","onl_cap3","onl_cap4","onl_cap5","onl_cap6")
names(df)[names(df) %in% c("Q23_1","Q23_2","Q23_3","Q23_4","Q23_5")] <- c("mdriving1", "mdriving2", "mdriving3", "mdriving4", "mdriving5")
names(df)[names(df) %in% c("Q20_1","Q20_2","Q20_3")] <- c("nw_explore1","nw_explore2","nw_explore3")
names(df)[names(df) %in% c("Q24_1","Q24_2","Q24_3","Q24_4")] <- c("perf1","perf2","perf3","perf4")
head(df)
sapply(df, class)
table(df$fsale_range)
df1 <- df #change value of column from character to factor
table(is.na(df$scope))

df1$fsale_range <- as.factor(df1$fsale_range)
levels(df1$fsale_range)
df1$fsale_range <- factor(df1$fsale_range, levels = c("Less than 25%","25% - 50%","50% - 75%","More than 75%"))


for (i in 1:nrow(df1)){
  if (df1$scope[i] == "Nordic"){
    df1$scope[i] <- "Nordic"
  } else {
    if (df1$scope[i] == "EU"){
      df1$scope[i] <- "EU"
    } else {
      if (df1$scope[i] == "Worldwide"){
        df1$scope[i] <- "Worldwide"
      } else{
        df1$scope[i] <- "NA"
      }
    }
  }
}

df1$scope1 <- df1$scope
for (i in 1:nrow(df1)){
  if (df1$scope1[i] == "Nordic"){
    df1$scope1[i] <- 1
  } else {
    if (df1$scope1[i] == "EU"){
      df1$scope1[i] <- 2
    } else {
      if (df1$scope1[i] == "Worldwide"){
        df1$scope1[i] <- 3
      } else{
        df1$scope1[i] <- NA
      }
    }
  }
}
df1$scope1 <- as.integer(df1$scope1)
class(df1$scope1)
mean(df1$scope1, na.rm = TRUE) #mean scope = 2.27 -> assign NA value = EU
df1$scope1 <- NULL

#Because missing values are few, and mean = 2.27, we assign mis
for (i in 1:nrow(df1)){
  if (df1$scope[i] == "NA"){
    df1$scope[i] <- "EU"
  }
}

df1$scope <- as.factor(df1$scope)
levels(df1$scope)
df1$scope <- factor(df1$scope, levels = c("Nordic","EU","Worldwide"))

fact_func <- function(x1,x2){
  x1 <- factor(x1, levels = x2)
  levels(x1)
}

fact_func(df1$size, c("Small","Medium"))
fact_func(df1$age, c("From 6 to 10 years","From 11 to 20 years","More than 20 years"))
fact_func(df1$int_exp, c("Less than 5 years","From 6 to 10 years","From 11 to 20 years","More than 20 years"))
fact_func(df1$osale_range, c("Less than 25%","25% - 50%","50% - 75%","More than 75%"))

#outliers check
## https://statsandr.com/blog/outliers-detection-in-r/

library(ggplot2)

ggplot(df1) +
  aes(x = perf1) +
  geom_histogram(bins = 10, fill = "#0c4c8a") +
  theme_minimal()

temp2 <- dplyr::filter(df1, perf1 >= 1)
colMeans(df[, c("perf1","perf2","perf3","perf4")])
temp1 <- dplyr::filter(df1, perf1 < 1)
temp1$perf1 <- 3
temp1$perf2 <- 3
temp1$perf3 <- 3
temp1$perf4 <- 3
df1 <- rbind(temp2,temp1)
nrow(df1)

#normal distribution check
## https://statsandr.com/blog/do-my-data-follow-a-normal-distribution-a-note-on-the-most-widely-used-distribution-and-how-to-test-for-normality-in-r/

#run model
library(dplyr)
library(tidyr)
names(df1)
df2 <- df1[, c(1:6,12:14,18:28,35:46)]
View(df2)

table(is.na(df2))

