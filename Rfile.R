# Identifying a statistically significant politically critical period surrounding African elections
# Negative binomial models and two-sample difference of mean tests

library(dplyr)
library(MASS)
library(ggplot2)
library(readxl)

full = read_xlsx('/Users/sithmasineka/Desktop/Research_3/R project/Project1/full.xlsx')
dff1 = data.frame(full)
head(dff1)

# Data pre-processing
print(paste('There are', nrow(dff1), 'political violence events in this data set'))
dff1$Since = ifelse(dff1$Since == '.', 1000000,dff1$Since)
dff1$Until[is.na(dff1$Until)] = -1000000
dff1$Since = as.integer(dff1$Since)
dff1 = dff1 %>% mutate(pre = ifelse((abs(Until) <= Since), 1,0))  #if Until<Since, then the event occured in the pre election period
dff1$post = ifelse(dff1$pre == 0, 1,0)

# Trend in incidence of political violence over the years
dff1$event_date = as.Date(dff1$event_date, '%Y/%m/%d')
dff1$freq = 1
dff2 = data.frame(dff1 %>%group_by(event_date) %>%summarise(sum_freq = sum(freq)))
head(dff2)
ggplot( data = dff2, aes( event_date, sum_freq )) + geom_line(colour = "dark blue") +xlab('Date') +ylab('Daily number of violent events') +
  ggtitle('Daily violence distribution in African countries')+stat_smooth(color = "red", method="loess",se=FALSE,size=0.5,linetype=2)+
  theme(axis.text=element_text(size=12), axis.title=element_text(size=14),title = element_text(size = 14))

dfall = dff1[((dff1$pre == 1) | (dff1$post == 1)) & (dff1$event_type!='Strategic developments'),c(4,6,7,17,18)]
dfall = dfall %>% mutate(days = ifelse(pre == 1,Until,Since))
head(dfall)

# Trend in incidence of political violence closer to election days
dfall %>% ggplot( aes(x=days)) +
  geom_histogram( binwidth=30, fill="dark blue", color="#e9ecef", alpha=0.9) +
  ggtitle("Distribution of violent events around the election") +xlab('Number of days to/from election day') +
  theme(axis.text=element_text(size=12),axis.title=element_text(size=14),title = element_text(size = 14))

Month_freq1 = read_xlsx('/Users/sithmasineka/Desktop/Research_3/R project/Project1/Month_freq1.xlsx')
df=data.frame(Month_freq1)
head(df)

df$std.logGDP=scale(log(df$GDP))
df$std.logpop=scale(log(df$population))
df$std.logtech=scale(log(df$tech))
df$Until_M=as.integer(df$Until_M)
df$Since_M=as.integer(df$Since_M)
df$polity=df$polity+10
df$Until_M=-1*(df$Until_M)

# Negative binomial regression models & two-sample difference of mean tests.

# Election cycle: One month - fifteen days before and after the elections.

df['Election']=NA 
df['Election']=df['Elections'] # coded 1 if this month falls in the one month of election period
neg.mod1=glm.nb(freq ~ factor(Election)+Until_M+Since_M+kill+torture+polity+fair+corrupt+factor(war)+std.logGDP+
                  std.logpop+std.logtech,data = df)
summary(neg.mod1)

eve=df[df$Election==1,] # dataframe of election period
Neve=df[df$Election!=1,] # dataframe of non-election period
print(paste0('Number of events in the election period is ', sum(eve$freq)))
print(paste0('Number of events in the non-election period is ', sum(Neve$freq)))
a=t.test(eve$freq, y = Neve$freq, var.equal = FALSE, conf.level = 0.95)
b=t.test(eve$freq, y = Neve$freq, var.equal = TRUE, conf.level = 0.95)
c=wilcox.test(eve$freq, y = Neve$freq, alternative = "two.sided")
r1=round(c(a$p.value,b$p.value,c$p.value),4)
print(paste0('The p-values for t-tests, Welch test, and Wilcoxon test are ', r1[1],', ',r1[2],' and ',r1[3],' respectively'))

# The function 'Neg.Models' can be used to fit negative binomial regression models. 
# Here, the input variables are period, pre and post which represent 
# months of election cycle, pre-election period and post-election period respectively.

Neg.Models = function(period,pre,post){
  frame = df
  frame['Election'] = NA # coded 1 if the month falls in the considered election cycle
  for (i in 1:length(frame$Elections)) {
    if (frame$Elections[i] == 1){
      frame$Election[c((i-pre):(i+post))] = rep(1,period)
    }
  }
  frame$Election[is.na(frame$Election)] = 0
  Model = glm.nb(freq ~ factor(Election)+Until_M+Since_M+kill+torture+polity+fair+corrupt+factor(war)+std.logGDP+
                   std.logpop+std.logtech,data = frame)
  return(list(summary(Model),frame))
}

# The function called as 'Mean_Tests' can be used to run two-sample difference of mean tests : 
# t-test, Welch test, and Wilcoxon test. Here, the input variable is the data frame created from the 
# 'Neg.Models' function

Mean_Tests = function(DF){
  event = DF[DF$Election == 1,] # dataframe of election period
  N_event = DF[DF$Election != 1,] # dataframe of non-election period
  print(paste0('Number of events in the election period is ', sum(event$freq)))
  print(paste0('Number of events in the non-election period is ', sum(N_event$freq)))
  a = t.test(event$freq, y = N_event$freq, var.equal = FALSE, conf.level = 0.95)
  b = t.test(event$freq, y = N_event$freq, var.equal = TRUE, conf.level = 0.95)
  c = wilcox.test(event$freq, y = N_event$freq, alternative = "two.sided")
  r = round(c(a$p.value,b$p.value,c$p.value),4)
  print(paste0('The p-values for t-tests, Welch test, and Wilcoxon test are ', r[1],', ',r[2],' and ',r[3],' respectively'))
}

# Election cycle: Four months - two months before and one month after election month plus election month itself.

cycle.4 = Neg.Models(period = 4, pre = 2, post = 1)
cycle.4[[1]]

Mean_Tests(DF = cycle.4[[2]])

# Election cycle: Six months - three months before and two months after election month plus election month itself.

cycle.6 = Neg.Models(period = 6, pre = 3, post = 2)
cycle.6[[1]]

Mean_Tests(DF = cycle.6[[2]])

# Election cycle: Eight months - four months before and three months after election month plus election month itself.

cycle.8 = Neg.Models(period = 8, pre = 4, post = 3)
cycle.8[[1]]

Mean_Tests(DF = cycle.8[[2]])


