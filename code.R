#Name: Anooshka Bajaj


#Data Analysis

#1
library (readxl)
r <- read_excel("E:/India_Health_Database.xlsx",range = "A1:C30")

for (i in 1:6)
{
pop <- (r[2])+((r[3]-r[2])*i/7)  #linearly extrapolating total population 

if (i==1)
{
  pop2012 <- pop
}
if (i==2)
{
  pop2013 <- pop
}
if (i==3)
{
  pop2014 <- pop
}
if (i==4)
{
  pop2015 <- pop
}
if (i==5)
{
  pop2016 <- pop
}
if (i==6)
{
  pop2017 <- pop
}
}
                                 #adding new population data in the existing data frame
r$Population2012 <- pop2012
r$Population2013 <- pop2013
r$Population2014 <- pop2014
r$Population2015 <- pop2015
r$Population2016 <- pop2016
r$Population2017 <- pop2017

r                                #updated data frame

library(tibble)
view(r)                          #to view full data frame


#2
f1 <- read_excel("E:/Sem 4/BE303 Applied Biostatistics/India_Health_Database.xlsx", sheet = 3, range = "A3:B32")     
f2 <- read_excel("E:/Sem 4/BE303 Applied Biostatistics/India_Health_Database.xlsx", sheet = 3, range = "E3:F32")     
f3 <- read_excel("E:/Sem 4/BE303 Applied Biostatistics/India_Health_Database.xlsx", sheet = 3, range = "K3:L32")     
f4 <- read_excel("E:/Sem 4/BE303 Applied Biostatistics/India_Health_Database.xlsx", sheet = 3, range = "Q3:R32")     
f5 <- read_excel("E:/Sem 4/BE303 Applied Biostatistics/India_Health_Database.xlsx", sheet = 3, range = "W3:X32")     
f6 <- read_excel("E:/Sem 4/BE303 Applied Biostatistics/India_Health_Database.xlsx", sheet = 3, range = "AC3:AD32")     
f7 <- read_excel("E:/Sem 4/BE303 Applied Biostatistics/India_Health_Database.xlsx", sheet = 3, range = "AI3:AJ32")   
                      
df <- data.frame(f1,f2,f3,f4,f5,f6,f7)          #creating a data frame of only female cases of pneumonia
view(df)
 
                                 #computing lethality of all states per year
lethality_2013 <- df[4]/df[3]
lethality_2014 <- df[6]/df[5]
lethality_2015 <- df[8]/df[7]
lethality_2016 <- df[10]/df[9]
lethality_2017 <- df[12]/df[11]
lethality_2018 <- df[14]/df[13]
                                 
df2 <- data.frame(lethality_2013,lethality_2014,lethality_2015,lethality_2016,lethality_2017,lethality_2018)      #creating a data frame of lethality values of each state from 2013 to 2018 
view(df2)

mean <- rowMeans(df2,na.rm=TRUE)      #finding average lethality of each state
mean

library(matrixStats)                 
df2_2 <- data.matrix(df2, rownames.force = NA)
sd <- rowSds(df2_2,na.rm=TRUE)        #finding SD of each state

alpha <- 0.05
degrees.freedom <- 29 - 1
t.score <- qt(p=alpha/2, df=degrees.freedom,lower.tail=F)

error_margin <- (t.score)*(sd/sqrt(29))
  
lowerconf <- mean - error_margin
upperconf <- mean + error_margin


confidence_interval <- upperconf-lowerconf      #finding confidence interval for each state 
confidence_interval - sd
confidence_interval + sd


m <- mean(as.matrix(df2),na.rm=TRUE)  #finding average lethality of all states  
m
s <- sd(as.matrix(df2),na.rm=TRUE)    #finding standard deviation

alpha <- 0.05
degrees.freedom <- 29 - 1
t.score <- qt(p=alpha/2, df=degrees.freedom,lower.tail=F)

error_margin1 <- (t.score)*(s/sqrt(29))

lowerconf1 <- m - error_margin1       #lower confidence value 
lowerconf1
upperconf1 <- m + error_margin1       #upper confidence value
upperconf1

confidence_interval1 <- upperconf1 - lowerconf1   #finding overall confidence interval
confidence_interval1 - s
confidence_interval1 + s


#3                   
mortality_2013 <- df[4]/r[5]     #computing mortality per year per state
mortality_2014 <-df[6]/r[6]
mortality_2015 <-df[8]/r[7]
mortality_2016 <-df[10]/r[8]
mortality_2017 <-df[12]/r[9]
mortality_2018 <-df[14]/r[3]

pneu_mortality <- data.frame(mortality_2013,mortality_2014,mortality_2015,mortality_2016,mortality_2017,mortality_2018)  #creating a data frame of mortality values of each state per year
view(pneu_mortality)


pneu_mortality1 <- data.matrix(pneu_mortality)
state_number <- data.matrix(df[2])
mort <- data.frame(state_number,pneu_mortality1)

mort_2013 <- data.matrix(pneu_mortality[1])
mort_2014 <- data.matrix(pneu_mortality[2])
mort_2015 <- data.matrix(pneu_mortality[3])
mort_2016 <- data.matrix(pneu_mortality[4])
mort_2017 <- data.matrix(pneu_mortality[5])
mort_2018 <- data.matrix(pneu_mortality[6])

ggplot(data=mort,aes(x=state_number)) + labs(title="Mortality per year and state", x="State Number", y="Mortality") + theme(plot.title = element_text(hjust = 0.5, size=12,face="bold")) + theme(axis.text=element_text(size=10), axis.title=element_text(size=10)) + geom_line(aes(y=mort_2013)) + geom_point(aes(y=mort_2013),size=0.5) + geom_line(aes(y=mort_2014),colour='red') + geom_point(aes(y=mort_2014),colour='red',size=0.5) + geom_line(aes(y=mort_2015),colour='blue') + geom_point(aes(y=mort_2015),colour='blue',size=0.5) + geom_line(aes(y=mort_2016),colour='orange') + geom_point(aes(y=mort_2016),colour='orange',size=0.5) + geom_line(aes(y=mort_2017),colour='green') + geom_point(aes(y=mort_2017),colour='green',size=0.5) + geom_line(aes(y=mort_2018),colour='brown') + geom_point(aes(y=mort_2018),colour='brown',size=0.5)                                           

q <- ggplot(data=mort,aes(x=state_number)) + labs(title="Mortality per year per state", x="State Number", y="Mortality") + theme(plot.title = element_text(hjust = 0.5, size=12,face="bold")) + theme(axis.text=element_text(size=10), axis.title=element_text(size=10)) + geom_line(aes(y=mort_2013,colour = "2013")) + geom_point(aes(y=mort_2013),size=0.5) + geom_line(aes(y=mort_2014,colour='2014')) + geom_point(aes(y=mort_2014),size=0.5) + geom_line(aes(y=mort_2015,colour='2015')) + geom_point(aes(y=mort_2015),size=0.5) + geom_line(aes(y=mort_2016,colour='2016')) + geom_point(aes(y=mort_2016),size=0.5) + geom_line(aes(y=mort_2017,colour='2017')) + geom_point(aes(y=mort_2017),size=0.5) + geom_line(aes(y=mort_2018,colour='2018')) + geom_point(aes(y=mort_2018),size=0.5)
q

#plotting graphs of years 2016 and 2017 separately for better understanding
#q1 <- ggplot(data=mort,aes(x=state_number)) + labs(title="Mortality per year per state", x="State Number", y="Mortality") + theme(plot.title = element_text(hjust = 0.5, size=12,face="bold")) + theme(axis.text=element_text(size=10), axis.title=element_text(size=10)) + geom_line(aes(y=mort_2013,colour = "2013")) + geom_point(aes(y=mort_2013),size=0.5) + geom_line(aes(y=mort_2014,colour='2014')) + geom_point(aes(y=mort_2014),size=0.5) + geom_line(aes(y=mort_2015,colour='2015')) + geom_point(aes(y=mort_2015),size=0.5) + geom_line(aes(y=mort_2018,colour='2018')) + geom_point(aes(y=mort_2018),size=0.5)
#q1

#q2 <- ggplot(data=mort,aes(x=state_number)) + labs(title="Mortality per year per state", x="State Number", y="Mortality") + theme(plot.title = element_text(hjust = 0.5, size=12,face="bold")) + theme(axis.text=element_text(size=10), axis.title=element_text(size=10)) + geom_line(aes(y=mort_2016,colour='2016')) + geom_point(aes(y=mort_2016),size=0.5) + geom_line(aes(y=mort_2017,colour='2017')) + geom_point(aes(y=mort_2017),size=0.5)
#q2


#4
sum_cases <- matrix(nrow = 29, ncol = 1)
for (k in (1:29))
{
   sum_cases[k] <- df[k,3]+df[k,5]+df[k,7]+df[k,9]+df[k,11]+df[k,13]
}
sum_cases                                                                  #total number of cases in a state over the years

pie(sum_cases,radius=1,main="Total pneumonia cases per state",cex=0.5)     #making pie chart with share of cases per state


#5
pop_avg_state <- rowMeans(r[c(3,5,6,7,8,9)])           #average population of each state from 2013 to 2018
case_avg_state <- rowMeans(df[c(3,5,7,9,11,13)])       #average number of pneumonia cases in each state from 2013 to 2018
df_pop_case <- data.frame(pop_avg_state,case_avg_state)

ggplot(df_pop_case, aes(x=pop_avg_state, y=case_avg_state))+geom_point() + geom_smooth (method='lm',se=FALSE) + labs(title="Population and Cases Correlation", x="Average population per state", y="Average cases per state") + theme(plot.title = element_text(hjust = 0.5, size=12,face="bold"))


#6
cor_1 <- NA
for (i in 1:5)
{
  cor1 <- cor(x=pneu_mortality[i],y=r[i+4],method="pearson")       #finding pearson correlation coefficient between mortality and population density for 2013 to 2017
  cor_1[i] <- cor1
}

cor2018 <- cor(x=pneu_mortality[6],y=r[3],method="pearson")        #finding pearson correlation coefficient between mortality and population density for 2018
append(cor_1, cor2018)                                             #correlation coefficient of years 2013 to 2018 

