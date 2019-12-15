######Using Statistics in Ecology 2019, homework part two ##########xxx

#„Generate a data set“ means that you should also describe briefly a biologically realistic setting which may have yielded such data, i.e. the variables must have names and the results should be meaningfully interpreted. Everything which is in the figures must be readable. Everyone will do his/her work independently, you are allowed to ask for advice but direct similarities will be punished. Excessive numerical accuracy is moderately punishable. Please copy the text of each task before you answers. Display your “a” value. 
a <- 8
#1. Generate biologically relevant data in which chi-squared test would reveal a statistically significant association in a 2x2 (two times two) frequency table. In one of the cells there should be value 6a. Present the table and the result both as copy-paste from R and as such a sentence which you would be OK for a research paper. Present a table with identical marginal distributions in which there is no significant association between the variables (this can you do by hand at home).

(petal_colour.anther_number <- matrix(c(a*6, 10,18,40),2,2,dimnames = list(c("five", "three"),c("red", "blue"))) )

#red blue
#five   48   18
#three  10   40
#chisq.test(petal_colour.anther_number)

#Pearson's Chi-squared test with Yates' continuity correction

#data:  petal_colour.anther_number
#X-squared = 29.562, df = 1, p-value = 5.414e-08

(petal_colour.anther_number.nonsignificat <- matrix(c(a*6, 34,42,40),2,2,dimnames = list(c("five", "three"),c("red", "blue"))))
#red blue
#five   48   42
#three  34   40

chisq.test(petal_colour.anther_number.nonsignificat)

#Pearson's Chi-squared test with Yates' continuity correction

#data:  petal_colour.anther_number.nonsignificat
#X-squared = 0.61562, df = 1, p-value = 0.4327




#2. Generate a data set in which two continuous independent variables are negatively correlated (-0.65>r>-0.85, present a graph, and a result of correlation analysis) and the dependent variable (sample mean between 5a and 6a) depends on both of these variables, when examined one at a time (present graphs). Perform type I analyses with both orders of the variables (do not include the interaction), present results as raw tables (copy-paste from R). Explain the difference in the interpretation of the results. Present the results also for type III analysis but this time as an edited ANOVA table (as you would present it in a research paper). Please do not draw more lines than it is customary for tables in research papers.######




q <- c(39 ,40 ,40 ,40 ,40 ,40 ,40 ,40 ,40 ,41 ,41 ,41 ,41 ,42 ,42, 42 ,42 ,42 ,42 ,43)  #dependent
x <- c(19,11,14,17,14,20,21,18,21,26,29,21,24,27,24,30,31,28,31,36) #independent
y <- c(30, 31,31,29,27, 29, 23,20,22,21,25,21,21,19,17,19,13,10,12,11)#independent

cor(x,y)
plot(x~y, pch= 19)
cor(q,x)
cor(q,y)

# Type I linear model
anova(lm(q~x+y, data=df))
summary(lm(q~x+y, data=df))
#plot(lm(q~x+y, data=df))

# Type I linear model - changed order
anova(lm(q~y+x, data=df))
summary(lm(q~y+x, data=df))
#plot(lm(q~y+x, data=df))

library("PerformanceAnalytics")

# Type III linear model
drop1(lm(q~y+x, data=df), test = "F")
drop1(lm(q~x+y, data=df), test = "F")

##second possibility
q <- round(sort(rnorm(20,sample(c((5*a):(6*a)),1))))  #dependent
x <- c(19,11,14,17,14,20,21,18,21,26,29,21,24,27,24,30,31,28,31,36) #independent
y <- c(30, 31,31,29,27, 29, 23,20,22,21,25,21,21,19,17,19,13,10,12,11)#independent

cor(x,y)
plot(x~y, pch= 19)
cor(q,x)
cor(q,y)

# Type I linear model
anova(lm(q~x+y, data=df))
summary(lm(q~x+y, data=df))
#plot(lm(q~x+y, data=df))

# Type I linear model - changed order
anova(lm(q~y+x, data=df))
summary(lm(q~y+x, data=df))
#plot(lm(q~y+x, data=df))

library("PerformanceAnalytics")

# Type III linear model
drop1(lm(q~y+x, data=df), test = "F")
drop1(lm(q~x+y, data=df), test = "F")









############ with function
trigger <- function(){
  simcor <- function (n, xmean, xsd, ymean, ysd,qmean, qsd, correlation) {
    q <- rnorm(n) #dependent
    x <- rnorm(n) #independent
    y <- rnorm(n) #independent
    z <- correlation * scale(x)[,1] + sqrt(1 - correlation^2) *
      scale(resid(lm(y ~ x)))[,1]
        w <- correlation * scale(q)[,1] + sqrt(1 + 0.9^2) *
      scale(resid(lm(x ~ q)))[,1]
    qresult <- qmean + qsd  * w
    xresult <- xmean + xsd * scale(x)[,1]
    yresult <- ymean + ysd * z
    dafa <-  data.frame(x=xresult,y=yresult,q=qresult)
  }
     df <- simcor(2000, 30, 2,40,10, 5.5*a,8,-0.70)
  colnames(df)
  cor(df$x, df$y)
  cor(df$x, df$q)
  cor(df$y, df$q)
  plot(df$y~df$x, ylab = "mean leaf size per plant [cm]", xlab = "root diameter [cm]", pch=19, ylim = c(0,80), xlim = c(0,40))
  

cor(df$x,df$y)

}

trigger()
shapiro.test(df$x)
shapiro.test(df$y)
cor.test(df$x,df$y)

# Type I linear model
anova(lm(q~x+y, data=df))
summary(lm(q~x+y, data=df))
#plot(lm(q~x+y, data=df))

# Type I linear model - changed order
anova(lm(q~y+x, data=df))
summary(lm(q~y+x, data=df))
#plot(lm(q~y+x, data=df))

library("PerformanceAnalytics")

# Type III linear model
drop1(lm(q~y+x, data=df), test = "F")
drop1(lm(q~x+y, data=df), test = "F")


#3. Generate data in which the results of a type I ANOVA with two discrete independent variables do not depend at all on the order of the variables in the model (recall which assumption should be met to achieve this). Do not include an interaction in the model. Please present the results with the two different orders as raw tables. Calculate the residuals and present graphically frequency distributions of the residuals for both levels of one of the independent variables. Are the assumptions of ANOVA met? (Tell why. No worries if not).######


# predictors shoud not be correlated (continuous variables) or unbalanced in case of two discrete variables



n<- 200    
co_dataset <- data.frame(row = 1:n)
co_dataset$f_visi <- sort(rnorm(n, 10,3)) #dependent - number of flower visitors per hour
co_dataset$petal_colour <- c(rep("red", n))
co_dataset$nectar_content <- c(rep("low", n))                 

co_dataset1 <- data.frame(row = 1:n)
co_dataset1$f_visi <- sort(rnorm(n, 5,3)) #dependent - number of flower visitors per hour
co_dataset1$petal_colour <- c(rep("blue", n))
co_dataset1$nectar_content <- c(rep("low", n))                 

co_dataset2 <- data.frame(row = 1:n)
co_dataset2$f_visi <- sort(rnorm(n, 40,3)) #dependent - number of flower visitors per hour
co_dataset2$petal_colour <- c(rep("red", n))
co_dataset2$nectar_content <- c(rep("high", n))


co_dataset3 <- data.frame(row = 1:n)
co_dataset3$f_visi <- sort(rnorm(n, 20,3)) #dependent - number of flower visitors per hour
co_dataset3$petal_colour <- c(rep("red", n))
co_dataset3$nectar_content <- c(rep("low", n))


fl_visitors <- rbind(co_dataset, co_dataset1, co_dataset2, co_dataset3)

boxplot(fl_visitors$f_visi~fl_visitors$petal_colour)

boxplot(iris$Sepal.Length~iris$Species)

    
  mod1 <- (lm(f_visi~nectar_content+petal_colour, data = fl_visitors))
anova(mod1)
summary(mod1)
plot(mod1)
fl_visitors$resid <- residuals(mod1)
hist(fl_visitors[fl_visitors$nectar_content == "low",]$resid,20, xlab = "residuals of nectar content for level low [??l]", main = "histogram of residuals")
shapiro.test(fl_visitors[fl_visitors$nectar_content == "low",]$resid)

hist(fl_visitors[fl_visitors$nectar_content == "high",]$resid,20, xlab = "residuals of nectar content for level high [??l]", main = "histogram of residuals")
shapiro.test(fl_visitors[fl_visitors$nectar_content == "high",]$resid)

mod2 <- (lm(f_visi~petal_colour+nectar_content, data = fl_visitors))
anova(mod2)
par(mfrow=c(2,2))
plot(mod2)


  #assumptions of anova are met. 
  #1 Independence of observations - lets say, that in thid case, I measured randomly selected flowers across Europe. 
  #2 Normality in distribution of resiuals - residuals are in this case normal - as you can see on the second diagnostic plot or on histogram of residuals. 
  #3  Homoscedasticity  - variance in levels of predictors should be more or les equal.

#4. Examine section „Analysis of covariance – one important application“ (topic 7) and present an example from the ecology of fish (what was studied and why?) in which the described logic applies. Discuss the setting and explain, how would you interpret the results of the analyses in which the covariate 1) is not, 2) is included in the model and in the last case 1) will or 2) will not attain significance. No need to perform the analysis but present a hypothetical table of results. 


#Our study investigate influence of nitrogen concentration in water on reproduction of fish. Nitrogen concentration in water was for long time in  suspicion for negative impact on fish populations. In this study was selected one species and number of eggs laid per female was measured by using a diving cylinder in artificial ponds, which do not differ in anything else than in nitrogen content. Length of females was used as covariate (all females in analysis was measured underwater in order to avoid any harm). Fortunately is length of females normaly distributed in all ponds. 





  par(mfrow=c(1,1))

#5. Generate data in which the relationship between two variables deviates significantly from linearity and the relationship is convex. The mean of the sample of the independent variable should be between 2a and 3a. Present the test of non-linearity (raw table), the graph and the equation of the relationship (edited). 

  
  nonlin <- function(n, term,meanx, yvar ){
    x <- (seq( from = meanx-n, to = meanx+n, length.out = n ))
    y <- x^term + rnorm(n,0,yvar) 
m2 <- lm(y ~ x + I(x**2))
plot(x,y,
     pch = 19,
     col = rgb(0,0,0,0.5)
     )

x0 <- seq( min(x),  max(x), length = n) 
# make predictions from model
y0 <- predict(  m2, newdata = data.frame( x = x0) )
#add line to the plot
lines(x0, y0,lwd = 3, col = "red") # modify line width and color

# coefficients of the model:
  print(list(coef(m2) ,anova(m2) ))

  }
  nonlin(1000, 2, 2.5*a, 80000)


#6. The thickness of birch leaves in shade, humid and warm conditions is a. Please propose the values of leaf thickness for all combinations of illumination (shade/sun), humidity (humid/dry) and temperature (warm/cold) so that there were one and only one two-factor interaction in the data, a three-way interaction should not be there either. Present the situation graphically (you may draw by hand). How would you verbally formulate a three-way interaction if it still was there?  
birch <- data.frame(
thick = c(8, 19, 10, 21, 6, 5, 8, 7)  ,
temp = c(rep("warm", 4), rep("cold", 4)),
sun = rep(c(rep("shade", 2), rep("sun", 2)),2),
wat = rep(c("humid","dry"),4)
)  

par(mfrow=c(1,3))

birch2 <- tapply(birch$thick, list(birch$temp, birch$sun), mean)
plot(y = birch2[,1], x= c(1,2), axes = F, xlim = c(0.5,2.5),type = "o",  ylim=c(0,25), pch = 19, ylab = "Birch leaves thickness", xlab = "")
points(y = birch2[,2],x =  c(1,2),pch = 19, col = "red", type = "o")
lines()
axis(2)
axis(1, at = 1:2, c("Cold", "Warm"))
legend(1.5, 25, legend=c("Sun", "Shade"),
       col=c("red", "blue"), lty=1, cex=1,
       box.lty=0)
box(lwd =2)
text(x=0.5,y= 24, "A" , cex = 2)


birch2 <- tapply(birch$thick, list(birch$temp, birch$wat), mean)
plot(y = birch2[,1], x= c(1,2), axes = F, xlim = c(0.5,2.5),  type = "o",ylim=c(0,25), pch = 19, ylab = "", xlab = "")
points(y = birch2[,2],x =  c(1,2),pch = 19, col = "red",  type = "o")
  axis(2)
  axis(1, at = 1:2, c("Cold", "Warm"))
  legend(1.5, 25, legend=c("Dry", "Humid"),
         col=c("red", "blue"), lty=1, cex=1,
         box.lty=0)
  box(lwd =2)
  
  text(x=0.5,y= 24, "B", cex = 2 )
  
  
  birch2 <- tapply(birch$thick, list(birch$sun, birch$wat), mean)
  plot(y = birch2[,1], x= c(1,2), axes = F, xlim = c(0.5,2.5),  type = "o",ylim=c(0,25), pch = 19, ylab = "", xlab = "")
  points(y = birch2[,2],x =  c(1,2),pch = 19, col = "red",  type = "o")
  axis(2)
  axis(1, at = 1:2, c("Shade", "Sun"))
  legend(1.5, 25, legend=c("Humid", "Dry"),
         col=c("red", "blue"), lty=1, cex=1,
         box.lty=0)
  box(lwd =2)
  
  text(x=0.5,y= 24, "C", cex = 2 )
  

  ### how to formulate three-way interaction:
  # Thickness is dependent on levels of all three predictors. Thickness in shade condition is dependent on levels of humidity and temperature 

  
#7. Describe a fictional situation from bird ecology in which you would use a mixed ANOVA with one  random (not “brood”, invent something different) and one fixed factor. Explain, why are the factors treated as fixed/random. Generate the data and present the result as a raw table. Would it be reasonable to alternatively treat one of the random factors as fixed. If not, then why? If yes, how would it change the interpretation of the results? 
  
  par(mfrow=c(1,1))
  
  
  k<- c(1:4)
  n<- 200    
  bi_df <- data.frame(row = 1:n)
  bi_df$mites <- ( abs(round(rnorm(n, 50,10)))) #dependent - number of flower visitors per hour
  bi_df$spec <- c(rep("A", n))
  bi_df$site <- paste("site",rep(c(k), each = n/length(k)))
  
  
  
  k<- c(1:4)
  n<- 200    
  bi_df1 <- data.frame(row = 1:n)
  bi_df1$mites <- sort( abs(round(rnorm(n, 70,11)))) #dependent - number of flower visitors per hour
  bi_df1$spec <- c(rep("B", n))
  bi_df1$site <- paste("site",rep(c(k), each = n/length(k)))
  
  bi <- rbind(bi_df1, bi_df)
  bi <- data.frame(bi)

  boxplot(bi$mites~bi$spec)

 
  library("lme4")
  library("nlme")
  
  
anova(  lmer(mites~spec +(1|site), data=bi))
anova(  lme(mites~spec , random = ~1|site, data=bi))
anova(  lm(mites~spec +site, data=bi))

  

#8. There were 6 turtles, 3 were fed with snails, 3 were not (two treatments). Shell thickness on the turtles was measured in one, two, three and four years starting from the beginning of the treatment. The researchers are interested whether there is some overall effect of the treatment on shell thickness, and whether the dynamics of shell thickness in time differs between the treatments. Please find the data as an excel sheet, first please analyse the data with an ordinary ANOVA (treatment and time) not accounting for the fact that the same individuals were repeatedly measured, and then as a correct repeated measurements ANOVA. Present the results of both analyses as raw tables, find the differences (also in degrees of freedom) and explain what do they come from. Please note that in repeated measurements ANOVA, the number of error degrees of freedom may differ for different effects. Formulate the result of the repeated measurements ANOVA as a biologically meaningful sentence. 
library(readxl)
tur <- read_excel("repedata18eng.xls")
tur
turmod <- lm(shell~treatment*time, data = tur)
anova(turmod)
summary(turmod)
plot(turmod)

turmod1 <- lme(shell~treatment*time, random = ~1|individual, data = tur)
anova(turmod1)
summary(turmod1)

turmod2 <- lme(shell~treatment*time, random = ~1|individual, data = tur, correlation = corAR1(form = ~time|individual))

anova(turmod2)
summary(turmod)




#9. Generate data which you would analyse using the method of logistic regression and where the parameter b is in the range -0.7<b<-0.5. Perform the analysis, present the figure, significance test in the raw form and the equation of the logistic function (edited). Present the result as a biologically informative sentence as you would do it in a research paper.  
log_cuv <- function(n, mean0, mean1, sd0, sd1, callx, cally){
  
x1 <- (c(rnorm(n, mean0,sd0),rnorm(n,mean1,sd1)))
y <- c(rep(0,n),rep(1,n))
xx <- data.frame(x1,y)
# the model
m5 <- glm(y ~ x1, # model formula
          family = binomial) # family of models
drop1(m5, test = "LRT") # Type III-ish likelihood ratio test for whole factors

plot(x1,y, pch=20, col = rgb(0,0,0,0.4), xlab = callx, ylab = cally)
# Let's define the association as R function

p <- function(x1) {
  exp(m5$coefficients[1]+m5$coefficients[2]*x1) / (1 + exp(m5$coefficients[1]+m5$coefficients[2]*x1))
}
curve(p, lwd = 2,add = T)
plotdat <- data.frame(x1=(0:n))
preddat <- predict(m5, newdata=plotdat, se.fit=TRUE)
with(preddat, lines(0:n, exp(fit+1.96*se.fit)/(1+exp(fit+1.96*se.fit)), lty=2))
with(preddat, lines(0:n, exp(fit-1.96*se.fit)/(1+exp(fit-1.96*se.fit)), lty=2))


print(list(summary(m5),
m5$coefficients[2]), anova(m5,test = "Chisq"))

}
log_cuv(100,40,35,3,3, "colora size [mm]","probability of anther smut disease presence on flower")

#10. Present a fictional case in which you would compare a Poisson distributed variable between two populations of monkeys (in one of the groups, let it be  ?? = 1 + a/10 (± 10%)) Generate respective data and present these distributions graphically. Present tests to show that the generated distributions do not deviate significantly from Poisson distribution. Present a test for the among-population differences in the values of these variables. Present the results of the test both as raw output and as a meaningful sentence. 
# we are observing of number of mating events per day in two groups of apes

ska1<- rpois(100, 1 + a/10 )
ska2 <-rpois(100, 3)
ska <- data.frame(mat_freq = c(ska1,ska2), group= c(rep("A",100), rep("B", 100)))

hist(ska[ska$group == "A",]$mat_freq, main = "Histogram of mating frequency in group A", xlab = "number of mating events per day")
hist(ska[ska$group == "B",]$mat_freq, main = "Histogram of mating frequency in group B", xlab = "number of mating events per day")


poisson.test(sum(ska[ska$group == "A",]$mat_freq), length(ska[ska$group == "A",]$mat_freq))
poisson.test(sum(ska[ska$group == "B",]$mat_freq), length(ska[ska$group == "B",]$mat_freq))
poisson.test(sum(ska$mat_freq), length(ska$mat_freq))

m6 <- glm(mat_freq ~ group, family = poisson, data = ska)
drop1(m6, test = "LRT")
anova(m6)

#11. Describe a situation when, studying domestic cats, you should „merge“ three variables into one principal component (please avoid overlap with the example presented in the lecture, invent something by yourself). Present the interpretation of the new variable. Why cannot it be measured directly? Generate the data and find out whether the new variable correlates with some fourth one (present raw results of the correlation analysis). Present a table showing the values of the original variables and the calculated PCA component score.


simcor <- function (n, xmean, xsd, ymean, ysd,qmean, qsd,emean, esd, correlation,correlation1,correlation2) {
  q <- rnorm(n) #
  x <- rnorm(n) #
  y <- rnorm(n) #
  e <- rnorm(n) #
  
  z <- correlation * scale(x)[,1] + sqrt(1 - correlation^2) *
    scale(resid(lm(y ~ x)))[,1]
  w <- correlation1 * scale(q)[,1] + sqrt(1 + correlation1^2) *
    scale(resid(lm(x ~ q)))[,1]
  r <- correlation2 * scale(e)[,1] + sqrt(1 + correlation2^2) *
    scale(resid(lm(x ~ e)))[,1]
  qresult <- qmean + qsd  * w
  xresult <- xmean + xsd * scale(x)[,1]
  yresult <- ymean + ysd * z
  eresult <- emean + esd * r
  dafa <-  data.frame(n_likes=round(xresult),hair_softness=round(yresult),n_stroking_ev_per_day=round(qresult), n_events_when_it_is_not_tryin_kill_owner_per_day = round(eresult))
}
df <- simcor(20, 30, 2,40,10, 5.5*a,8,20,7,0.70,0.80,0.6)



cuteness <- prcomp(df[2:4])
df <- cbind(df, cuteness$x)

shapiro.test(cuteness$x[,1])
shapiro.test(df$n_likes)
cor.test(cuteness$x[,1], df$n_likes)

write.csv(df,"cats.csv")


#12. Describe a ficti onal ecological study in which you have studied the dependence of one continuous variable on another, so that the values of both variables have recorded from 20 sites on Kihnu island. There should be spatial autocorrelation (SAC) in the independent variable but not in the dependent variable. Present a map displaying the values of both variables for all study sites. Present a verbal interpretation of the SAC. What may have caused the SAC?
library("vegan")
library("leaflet")

simcor <- function (n, seedmean, seedsd) {
  lon = sample(seq(23.97,24.0001, by= 0.00001 ),n) 
  seed <-round( rnorm(n, seedmean, seedsd))
  lat = seq(58.11,58.1522,  length.out = n )
  
  ncon <- seq(1,n)
  dafa <-  data.frame(lon, lat, seed, ncon )
  dafa[dafa$lat > 58.13, ]$ncon <- rnorm(length(dafa[dafa$lat > 58.13, ]$ncon), 60,10)
  dafa[dafa$lat <=58.13, ]$ncon <- rnorm(length(dafa[dafa$lat <= 58.13, ]$ncon), 20,10)
df<- data.frame(dafa)
  }

df <- simcor(n=20, seedmean=30, seedsd=8)

ncond <-dist(df$ncon) 
di <- vegdist(log1p(df), method = "euclidean")
plot(ncond, di)
mantel(ncond, di)
m.c1=mantel.correlog(di, ncond)


plot(df$lon, df$lat, type = "n")
points(df$lon, df$lat, cex = df$ncon/10)

m <- leaflet() %>% setView(lng = 23.9789, lat = 58.1301, zoom = 12.5)

see <- df$seed

m %>% addTiles()
leaflet(df) %>% addTiles() %>%
  addCircleMarkers(
    radius = (df$ncon/3),
    color = pal(df$seed),
    stroke = FALSE, fillOpacity = 0.3,
    label =  as.character( df$seed),
    labelOptions = labelOptions(noHide = T, direction = 'top', textOnly = T)

  )



m %>% addTiles()
leaflet(df) %>% addTiles() %>%
  addCircleMarkers(
    radius = (df$seed/3),
    color = "red",
    stroke = FALSE, fillOpacity = 0.3,
    label =  as.character( df$seed),
    labelOptions = labelOptions(noHide = T, direction = 'top', textOnly = T)
    
  )
colfunc(n)



#*******************************************  end of tasks  *******************
  
  








