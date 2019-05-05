####FOLKS :

library(foreign)
library(MASS)
library(psych)

install.packages("stargazer")
install.packages("dummies")
install.packages("psych")
summary(UKdata)
summary(GERdata)

#Changing the factor variable to a dummy (numerical) variable 

dummy.party <- dummy.code(GERdata$party_affiliation)
head(dummy.party, n = 209)
GERdata<- data.frame(dummy.party,GERdata)

GERdata$list_candidate<-as.numeric(as.factor(GERdata$list_candidate))

GERdata$list_candidate[GERdata$list_candidate==1]<-0 #for the district MPs
GERdata$list_candidate[GERdata$list_candidate==2]<-1 #for the list MPs

#Filling the NAs in the ideological distance with the median
GERdata$ideological_distance[is.na(GERdata$ideological_distance)] <- median(GERdata$ideological_distance, na.rm = TRUE)

UKdata$ideological_distance[is.na(UKdata$ideological_distance)] <- median(UKdata$ideological_distance, na.rm=TRUE)

median(GERdata$number_speeches)
mean(GERdata$number_speeches)
median(UKdata$number_speeches)
mean(UKdata$number_speeches)
median(GERdata$ideological_distance)
median(UKdata$ideological_distance)

sum(UKdata$number_speeches)/48 #On average, 548 speeches were made in the UK during 2001-2005
sum(GERdata$number_speeches)/48 #On average, 80 speeches were made in Germany during 2005-2009

#Deleting the NAs in the data set, and change it to median of the collumn. I change it 
#to median because when I excluded the NAs then the ideological distance becomes insignificant
#in the regressions and I chose median not to be affected by the outliers.


The dependent variable is number of parliamentary speeches given between 2005-2009 in Germany
and 2001-2005 in the UK. In total for Germany, we have 209 MPs and for the UK we have 169 
MPs. As it can be seen from the hypotheses given by the literature, the main independent variable
is the ideological distance of MP to the partys position.The control variables for Germany and for 
the UK is different because of the change in the electoral systems.The UKuses majoritarian system 
for the elections and Germany uses proportional system. But one common control variable for both 
countries is the MP-being-the party leader. 


hist(GERdata$ideological_distance, main = "Ideological distance in German Parliament",xlab = "ideological distance",
     xlim = c(0,1), ylim = c(0,100)) #skewed to the right, not normally distributed,most of the MPs are
#cumulated between 0 and 0.2 ideological distance from the party leaders.
hist(GERdata$list_candidate, main = "List and District MPs in German Parliament", xlab="District(left) vs List(right)")
hist(GERdata$number_speeches, main = "Number of speeches in German Parliament", xlab = "number of speeches", ylim = c(0,80))
pairs(~ideological_distance+party_leader+number_speeches+party_affiliation, data = GERdata, main="Germany")
plot(GERdata$ideological_distance, GERdata$number_speeches, main = "Ideological distance and number of speeches",
     xlab = "ideological distance in Germany", ylab = "number of speech")

hist(UKdata$ideological_distance, main = "Ideological distance in the British Parliament",
     xlim = c(0,6), ylim = c(0,50)) #we can say that there are more ideological distances for the MPs in 
#the British Parliament if we compare it to Germany. But there is not any patter other than the one mentioned above 
#for the British MPs' ideological distances. 
hist(UKdata$conservative_MP, main = "Conservative and Non-Conservative MPs", xlab = "Conservative(1)/Non-Conservative(0)",
     ylim=c(0,120))
hist(UKdata$number_speeches, main = "Number of Speeches in the UK", xlab = "number of speeches")
pairs(~ideological_distance+party_leader+number_speeches+conservative_MP, data=UKdata, main="the UK")
#In the scatterplot above we can see the pattern between variables that ae shared in both countries.

plot(UKdata$ideological_distance, UKdata$number_speeches, main = "Ideological distance and number of speeches in the UK",
     xlab = "ideological distance", ylab = "number of speech")

From the very basic scatter plots with the linear regression line of effect of ideological distance 
on number of speeches in Germany and in the UK above, we see that there is a declining trend in Germany 
for the number of speeches as the ideological distance from the party ideology gets larger. For the UK,
we see the opposite of this trend, as the MPs in the British parliament move away from the party ideology, 
their number of speech increases. However, we do not know if it is a statistically significant trend for 
those electoral systems, and we do not know if linear model is the right model to test the hypotheses. 



hist(UKdata$number_speeches)#the number of speeches are higher in the UK than Germany

#Ideological distance and number of speeches in both countries are not normally distributed as we can see from
#the histograms.For both countries, to find the best model, I will first look at the type of dependent variable, whether 
#it is a count or binary data.

#Again, our dependent variable is the number of speeches for MPs in the parliament, so it is a count 
#variable. Therefore, I will offer to use the count models, which are poisson model and negative
#binomial model. To decide which one is useful to test the hypotheses stated, first I am going to run a poisson and 
#a negative binomial regressions for both countries' number of speeches including all the variables in their dataset
#to see the values of the variance and the mean.Negative binomial models assume the conditional means are not equal to the conditional 
#variances. This inequality is captured by estimating a dispersion parameter that is held 
#constant in a Poisson model. Thus, the Poisson model is actually nested in the negative 
#binomial model. We can then use a likelihood ratio test to compare these two and test this 
#model assumption.

##########For Germany:proportional system
pois.ger<- glm(number_speeches~ideological_distance+party_leader+party_affiliation+list_candidate+
                   committee+caolMPoutside, data = GERdata, family = "poisson")

negbin.ger<-glm.nb(number_speeches~ideological_distance+party_leader+SPD+CDU.CSU+BÜNDNIS.90.DIE.GRÜNEN+DIE.LINKE.+list_candidate+
                       committee+caolMPoutside, data = GERdata, control = glm.control(maxit = 100))
summary(pois.ger)
summary(negbin.ger)

options(scipen=999)
dispersion.ger<-2*(logLik(negbin.ger)-logLik(pois.ger))
dispersion.ger > qchisq(0.95, df = 1) #TRUE



#These tests above strongly suggest the negative binomial model for both countries full models(with all variables), 
#estimating the dispersion parameter, as being more appropriate than the Poisson model. But before looking at the 
#full model with all covariates, I want to create a basic model and see how ideological distance as the 
#main independent variable in the hypotheses affect estimates in different models as I will present on the regression
#table.

#Creating a random base model for Germany to see which model, poisson or negative binomial is sense to use.

base.ger1<-glm(number_speeches~list_candidate, data = GERdata, family = "poisson")#poisson

base.ger2<-glm.nb(number_speeches~list_candidate, data = GERdata, control = glm.control(maxit = 100))#negative binomial

dispersion<-2*(logLik(base.ger2)-logLik(base.ger1))
dispersion > qchisq(0.95, df=1)

I am going to use negative binomial for the different modelings of Germany again, in accordance with the likelihood ratio test

#Generating different models
model1.ger<-glm.nb(number_speeches~list_candidate, data = GERdata,
                 control = glm.control(maxit=100)) #caolMPoutside variable is insignificant and has a really low estimate (0.05) if I include it, so I kicked it out for the base.
model2.germ<-glm.nb(number_speeches~list_candidate+ideological_distance, data=GERdata, 
                   control = glm.control(maxit = 100)) #the model 2 with ideological distance and being party leader
model3.germ<-glm.nb(number_speeches~list_candidate+ideological_distance+caolMPoutside, data=GERdata,
                    control = glm.control(maxit=100))
model4.germ<-glm.nb(number_speeches~list_candidate+ideological_distance+caolMPoutside+party_leader+committee+SPD+CDU.CSU+BÜNDNIS.90.DIE.GRÜNEN+DIE.LINKE., 
                    data = GERdata, control=glm.control(maxit = 100))#full model

summary(model1.ger)
summary(model2.germ)
summary(model3.germ)
summary(model4.germ)

library(stargazer)

stargazer(list(model1.ger,model2.germ,model3.germ,model4.germ),out = "pois.negbin.tex",
          title="Negative Binomial Modelling the ideological distance in German Parliament",
          notes="Missing Values in the dataset are replaced by the median of the collumn with missing values",
          intercept.bottom=TRUE,
          covariate.labels=c("List Candidate","Ideological Distance","MP Outside Coalition","party leader","Committee Assignment",
                             "SPD","CDU/CSU","Greens","Die Linke","Constant"))

anova(model1.ger,model2.germ,model3.germ,model4.germ)#testing the log lik ratios 

Model 4 is better to use because it has a bigger loglik value in the negative binomial regression

#the simulations for the germany with using full model (model4)

model4.germ<-glm.nb(number_speeches~ideological_distance+party_leader+list_candidate+committee+SPD+CDU.CSU+BÜNDNIS.90.DIE.GRÜNEN+DIE.LINKE.+caolMPoutside, 
                    data = GERdata, control=glm.control(maxit = 100))#full model
summary(model4.germ)
nsim <- 1000
gammahat.ger <- coef(model4.germ)
Vhat.ger <- vcov(model4.germ)
S.ger <- mvrnorm(nsim, gammahat.ger, Vhat.ger)
dim(S.ger)

#the range for our main independent 
range(GERdata$ideological_distance)#is between 0 and 0.83

ideological.distance.sim.ger <- seq(0,1, length.out = 100)

scenario1 <- cbind(1, ideological.distance.sim.ger, 0, median(GERdata$list_candidate),
      median(GERdata$committee),1,0,0,0,1) #not party leaders=0, outsidecoalition=1, SPD
scenario3 <- cbind(1, ideological.distance.sim.ger, 0, median(GERdata$list_candidate),
      median(GERdata$committee),0,0,0,1,1)#Die Linke
scenario5 <- cbind(1, ideological.distance.sim.ger, 0, median(GERdata$list_candidate),
      median(GERdata$committee),0,1,0,0,1)#CDU
scenario7 <- cbind(1, ideological.distance.sim.ger, 0, median(GERdata$list_candidate),
      median(GERdata$committee),0,0,1,0,1)#Greens
scenario9 <- cbind(1, ideological.distance.sim.ger, 0, median(GERdata$list_candidate),
      median(GERdata$committee),0,0,0,0,1)#FDP
#SPD
Xbeta1 <- S.ger %*% t(scenario1)
lambda1 <- exp(Xbeta1)
p.mean.ger <- apply(lambda1, 2, mean)
p.qu.ger <- t(apply(lambda1, 2, quantile, prob = c(0.025, 0.975)))
#Die Linke
Xbeta3 <- S.ger %*% t(scenario3)
lambda3 <- exp(Xbeta3)
p.mean.ger.3 <- apply(lambda3, 2, mean)
p.qu.ger.3 <- t(apply(lambda3, 2, quantile, prob = c(0.025, 0.975)))
#CDU
Xbeta5 <- S.ger %*% t(scenario5)
lambda5 <- exp(Xbeta5)
p.mean.ger.5 <- apply(lambda5, 2, mean)
p.qu.ger.5 <- t(apply(lambda5, 2, quantile, prob = c(0.025, 0.975)))
#Greens
Xbeta7 <- S.ger %*% t(scenario7)
lambda7 <- exp(Xbeta7)
p.mean.ger.7 <- apply(lambda7, 2, mean)
p.qu.ger.7 <- t(apply(lambda7, 2, quantile, prob = c(0.025, 0.975)))
#FDP
Xbeta9 <- S.ger %*% t(scenario9)
lambda9 <- exp(Xbeta9)
p.mean.ger.9 <- apply(lambda9, 2, mean)
p.qu.ger.9 <- t(apply(lambda9, 2, quantile, prob = c(0.025, 0.975)))


#SDP plot
pdf(file = "SDP First simulation")
plot(ideological.distance.sim.ger, p.mean.ger, type="n",
     ylim = c(0,70),
     xlim = c(0,1),
     ylab = "legislative speeches",
     xlab = "ideological distance of SPD MPs",
     main = "Effect of Ideological Distance on Number of Speeches",
     bty = "n",
     las = 1)

polygon(c(rev(ideological.distance.sim.ger),ideological.distance.sim.ger), c(rev(p.qu.ger[,2]), p.qu.ger[,1]),
        col = "gray80",
        border = NA)
lines(ideological.distance.sim.ger, p.mean.ger, lwd = 2)
lines(ideological.distance.sim.ger, p.qu.ger[, 1], lty = "dashed", col = "gray20")
lines(ideological.distance.sim.ger, p.qu.ger[, 2], lty = "dashed", col = "gray20")
dev.off()

#Die Linke
range(p.mean.ger.3)
pdf(file = "Die Linke first simulation")
plot(ideological.distance.sim.ger, p.mean.ger.3, type="n",
     ylim = c(0,70),
     xlim = c(0,1),
     ylab = "legislative speeches",
     xlab = "ideological distance of Die Linke MPs",
     main = "Effect of Ideological Distance on Number of Speeches",
     bty = "n",
     las = 1)

polygon(c(rev(ideological.distance.sim.ger),ideological.distance.sim.ger), c(rev(p.qu.ger.3[,2]), p.qu.ger.3[,1]),
        col = "gray80",
        border = NA)
lines(ideological.distance.sim.ger, p.mean.ger.3, lwd = 2)
lines(ideological.distance.sim.ger, p.qu.ger.3[, 1], lty = "dashed", col = "gray20")
lines(ideological.distance.sim.ger, p.qu.ger.3[, 2], lty = "dashed", col = "gray20")
dev.off()

#CDU
range(p.mean.ger.5)

pdf(file = "CDU first simulation")
plot(ideological.distance.sim.ger, p.mean.ger.5, type="n",
     ylim = c(0,70),
     xlim = c(0,1),
     ylab = "legislative speeches",
     xlab = "ideological distance of CDU MPs",
     main = "Effect of Ideological Distance on the Number of Speeches",
     bty = "n",
     las = 1)

polygon(c(rev(ideological.distance.sim.ger),ideological.distance.sim.ger), c(rev(p.qu.ger.5[,2]), p.qu.ger.5[,1]),
        col = "gray80",
        border = NA)
lines(ideological.distance.sim.ger, p.mean.ger.5, lwd = 2)
lines(ideological.distance.sim.ger, p.qu.ger.5[, 1], lty = "dashed", col = "gray20")
lines(ideological.distance.sim.ger, p.qu.ger.5[, 2], lty = "dashed", col = "gray20")
dev.off()

#Greens
pdf(file = "Greens first simulation")
plot(ideological.distance.sim.ger, p.mean.ger.7, type="n",
     ylim = c(0,70),
     xlim = c(0,1),
     ylab = "legislative speeches",
     xlab = "ideological distance of Greens MPs",
     main = "Effect of Ideological Distance on the Number of Speeches",
     bty = "n",
     las = 1)

polygon(c(rev(ideological.distance.sim.ger),ideological.distance.sim.ger), c(rev(p.qu.ger.7[,2]), p.qu.ger.7[,1]),
        col = "gray80",
        border = NA)
lines(ideological.distance.sim.ger, p.mean.ger.7, lwd = 2)
lines(ideological.distance.sim.ger, p.qu.ger.7[, 1], lty = "dashed", col = "gray20")
lines(ideological.distance.sim.ger, p.qu.ger.7[, 2], lty = "dashed", col = "gray20")
dev.off()
#FDP
pdf(file = "FDP first simulation")
plot(ideological.distance.sim.ger, p.mean.ger.9, type="n",
     ylim = c(0,70),
     xlim = c(0,1),
     ylab = "legislative speeches",
     xlab = "ideological distance of FDP MPs",
     main = "Effect of Ideological Distance on the Number of Speeches",
     bty = "n",
     las = 1)

polygon(c(rev(ideological.distance.sim.ger),ideological.distance.sim.ger), c(rev(p.qu.ger.9[,2]), p.qu.ger.9[,1]),
        col = "gray80",
        border = NA)
lines(ideological.distance.sim.ger, p.mean.ger.9, lwd = 2)
lines(ideological.distance.sim.ger, p.qu.ger.9[, 1], lty = "dashed", col = "gray20")
lines(ideological.distance.sim.ger, p.qu.ger.9[, 2], lty = "dashed", col = "gray20")
dev.off()

#After the simulations and expected value plots shows us that the backbenchers who are not the party leaders but having less ideological distance
#to party leadership are on average giving more speeches than the backbenchers who have a bigger ideological distance(simulated for PSD members).

#to get the idea for being a district member and list member, I will run a different simulations regarding being 
#district MP and list MP using the same model 4... by this way, i can compare it with majoritarian UK
model4.germ<-glm.nb(number_speeches~ideological_distance+party_leader+list_candidate+committee+SPD+CDU.CSU+BÜNDNIS.90.DIE.GRÜNEN+DIE.LINKE.+caolMPoutside, 
                    data = GERdata, control=glm.control(maxit = 100))#full model
summary(model4.germ)
nsim <- 1000
gammahat.ger <- coef(model4.germ)
Vhat.ger <- vcov(model4.germ)
S.ger <- mvrnorm(nsim, gammahat.ger, Vhat.ger)
dim(S.ger)

#FOR SPD
scenario.district.SPD<- cbind(1, mean(GERdata$ideological_distance),0,0,median(GERdata$committee),1,0,0,0,1)#being district MP, not party leader, being outside the coalition, CDUmember

scenario.list.SPD<- cbind(1,mean(GERdata$ideological_distance),0,1, median(GERdata$committee),1,0,0,0,1)#being list MP, not party leader,being outside of the coalition, CDUmember

dim(scenario.district.SPD)
district.beta.SPD <- S.ger %*% t(scenario.district.SPD)
lambda.district.SPD <- exp(district.beta.SPD)
dim(lambda.district.SPD)

list.beta.SPD<- S.ger %*% t(scenario.list.SPD)
lambda.list.SPD<- exp(list.beta.SPD)

#First differences of expected values in SPD
fd.SPD<-lambda.list.SPD-lambda.district.SPD
mean(fd.SPD)
range(fd.SPD)
quants.mean.fun <- function(x){
  c(quants=quantile(x,probs = c(0.025,0.975)), mean=mean(x))
}
quants.fd.SPD <- apply(as.matrix(fd.SPD), 2, quants.mean.fun)

plot (density(fd.SPD),xlim = c(-5,10), main="First Differences", 
      xlab="First differences between list and district MPs of SPD",
      ylab="density of the first differences")
abline(v=mean(fd.SPD))
abline(v = c(quants.fd.SPD[, 1]), lty = 2, col = "black")

#Going for the predicted values and ggplot
theta.ger <- model4.germ$theta #theta comes from the model 4(full model) fro each ggplot below also
exp.district.SPD <- sapply(lambda.district.SPD, function(x) mean(rnbinom(1000, size = theta.ger, mu = x))) 
exp.list.SPD <- sapply(lambda.list.SPD, function(x) mean(rnbinom(1000, size = theta.ger, mu = x)))

exp.values.SPD <- c(exp.district.SPD, exp.list.SPD)
df.SPD<- data.frame(exp.values.SPD)
df.SPD$id <- c(rep("District", 1000), rep("List", 1000))

install.packages("ggplot2")
library(ggplot2)

pdf(file = "ListDistrictSPD")
ggplot(df.SPD, aes(x = exp.values.SPD, fill = id)) + 
  geom_density(alpha = 0.4) +
  guides(fill = guide_legend(title = "List-District MPs")) +
  xlab("number of speeches of SPD MPs") +
  ylab("Density") + 
  theme_bw()
dev.off()
#FOR CDU
scenario.district.CDU <- cbind(1, mean(GERdata$ideological_distance),0,0,median(GERdata$committee),0,1,0,0,1)#being district MP, not party leader, being outside the coalition, CDUmember

scenario.list.CDU<- cbind(1,mean(GERdata$ideological_distance),0,1, median(GERdata$committee),0,1,0,0,1)#being list MP, not party leader,being outside of the coalition, CDUmember

dim(scenario.district.CDU)
district.beta.CDU <- S.ger %*% t(scenario.district.CDU)
lambda.district.CDU <- exp(district.beta.CDU)
dim(lambda.district.CDU)

list.beta.CDU<- S.ger %*% t(scenario.list.CDU)
lambda.list.CDU<- exp(list.beta.CDU)

fd.CDU<-lambda.list.CDU-lambda.district.CDU
mean(fd.CDU)
range(fd.CDU)
quants.mean.fun <- function(x){
  c(quants=quantile(x,probs = c(0.025,0.975)), mean=mean(x))
}
quants.fd.CDU <- apply(as.matrix(fd.CDU), 2, quants.mean.fun)

plot (density(fd.CDU),xlim = c(-4,8), main="First Differences", 
      xlab="First differences between list and district MPs of CDU",
      ylab="density of the first differences")
abline(v=mean(fd.CDU))
abline(v = c(quants.fd.CDU[, 1]), lty = 2, col = "black")

#Going for the predicted value and ggplot
theta.ger <- model4.germ$theta
exp.district.CDU <- sapply(lambda.district.CDU, function(x) mean(rnbinom(1000, size = theta.ger, mu = x))) 
exp.list.CDU <- sapply(lambda.list.CDU, function(x) mean(rnbinom(1000, size = theta.ger, mu = x)))

exp.values.CDU <- c(exp.district.CDU, exp.list.CDU)
df.CDU<- data.frame(exp.values.CDU)
df.CDU$id <- c(rep("District", 1000), rep("List", 1000))

pdf(file = "ListDistrictCDU")
ggplot(df.CDU, aes(x = exp.values.CDU, fill = id)) + 
  geom_density(alpha = 0.4) +
  guides(fill = guide_legend(title = "List and District MPs")) +
  xlab("number of speeches of CDU MPs") +
  ylab("Density") + 
  theme_bw()
dev.off()
#FOR DIE LINKE
scenario.district.DL <- cbind(1, mean(GERdata$ideological_distance),0,0,median(GERdata$committee),0,0,0,1,1)#being district MP, not party leader, being outside the coalition, CDUmember

scenario.list.DL<- cbind(1,mean(GERdata$ideological_distance),0,1, median(GERdata$committee),0,0,0,1,1)#being list MP, not party leader,being outside of the coalition, CDUmember

dim(scenario.district.DL)
district.beta.DL <- S.ger %*% t(scenario.district.DL)
lambda.district.DL <- exp(district.beta.DL)
dim(lambda.district.DL)

list.beta.DL<- S.ger %*% t(scenario.list.DL)
lambda.list.DL<- exp(list.beta.DL)


theta.ger <- model4.germ$theta
exp.district.DL<- sapply(lambda.district.DL, function(x) mean(rnbinom(1000, size = theta.ger, mu = x))) 
exp.list.DL<- sapply(lambda.list.DL, function(x) mean(rnbinom(1000, size = theta.ger, mu = x)))

exp.values.DL <- c(exp.district.DL, exp.list.DL)
df.DL<- data.frame(exp.values.DL)
df.DL$id <- c(rep("District", 1000), rep("List", 1000))

pdf(file = "ListDistrictDL")
ggplot(df.DL, aes(x = exp.values.DL, fill = id)) + 
  geom_density(alpha = 0.4) +
  guides(fill = guide_legend(title = "List and District MPs")) +
  xlab("number of speeches of Die Linke MPs") +
  ylab("Density") + 
  theme_bw()
dev.off()

#FOR GREENS
scenario.district.GR <- cbind(1, mean(GERdata$ideological_distance),0,0,median(GERdata$committee),0,0,1,0,1)#being district MP, not party leader, being outside the coalition, CDUmember

scenario.list.GR<- cbind(1,mean(GERdata$ideological_distance),0,1, median(GERdata$committee),0,0,1,0,1)#being list MP, not party leader,being outside of the coalition, CDUmember

dim(scenario.district.GR)
district.beta.GR <- S.ger %*% t(scenario.district.GR)
lambda.district.GR <- exp(district.beta.GR)
dim(lambda.district.GR)

list.beta.GR<- S.ger %*% t(scenario.list.GR)
lambda.list.GR<- exp(list.beta.GR)


theta.ger <- model4.germ$theta
exp.district.GR <- sapply(lambda.district.GR, function(x) mean(rnbinom(1000, size = theta.ger, mu = x))) 
exp.list.GR <- sapply(lambda.list.GR, function(x) mean(rnbinom(1000, size = theta.ger, mu = x)))

exp.values.GR<- c(exp.district.GR, exp.list.GR)
df.GR<- data.frame(exp.values.GR)
df.GR$id <- c(rep("District", 1000), rep("List", 1000))

pdf(file = "ListDistrictGreen")
ggplot(df.GR, aes(x = exp.values.GR, fill = id)) + 
  geom_density(alpha = 0.4) +
  guides(fill = guide_legend(title = "List vs District MPs")) +
  xlab("number of speeches of the Greens MPs") +
  ylab("Density") + 
  theme_bw()
dev.off()
#FOR FDP
scenario.district.FDP <- cbind(1, mean(GERdata$ideological_distance),0,0,median(GERdata$committee),0,0,0,0,1)#being district MP, not party leader, being outside the coalition, CDUmember

scenario.list.FDP<- cbind(1,mean(GERdata$ideological_distance),0,1, median(GERdata$committee),0,0,0,0,1)#being list MP, not party leader,being outside of the coalition, CDUmember

dim(scenario.district.FDP)
district.beta.FDP <- S.ger %*% t(scenario.district.FDP)
lambda.district.FDP <- exp(district.beta.FDP)
dim(lambda.district.FDP)

list.beta.FDP<- S.ger %*% t(scenario.list.FDP)
lambda.list.FDP<- exp(list.beta.FDP)


theta.ger <- model4.germ$theta
exp.district.FDP <- sapply(lambda.district.FDP, function(x) mean(rnbinom(1000, size = theta.ger, mu = x))) 
exp.list.FDP <- sapply(lambda.list.FDP, function(x) mean(rnbinom(1000, size = theta.ger, mu = x)))

exp.values.FDP <- c(exp.district.FDP, exp.list.FDP)
df.FDP<- data.frame(exp.values.FDP)
df.FDP$id <- c(rep("District", 1000), rep("List", 1000))

pdf(file = "ListDistrictFDP")
ggplot(df.FDP, aes(x = exp.values.FDP, fill = id)) + 
  geom_density(alpha = 0.4) +
  guides(fill = guide_legend(title = "List and District MPs")) +
  xlab("number of speeches of FDP MPs") +
  ylab("Density") + 
  theme_bw()
dev.off()


#The whole district members in the German Bundestag
exp.germ.district<- c(exp.district.CDU,exp.district.DL,exp.district.FDP,exp.district.GR,exp.district.SPD)
df.germ.district<- data.frame(exp.germ.district)
df.germ.district$id <- c(rep("CDU", 1000), rep("Die Linke", 1000), rep("FDP",1000), rep("Greens",1000),rep("SPD",1000))

pdf(file = "WholeDistrictMPs")
ggplot(df.germ.district, aes(x = exp.germ.district, fill = id)) + 
  geom_density(alpha = 0.4) +
  guides(fill = guide_legend(title = "All district-MPs from each party")) +
  xlab("number of speeches") +
  ylab("Density") + 
  theme_bw()
dev.off()

#The whole list members in the German Bundestag
exp.germ.list<- c(exp.list.CDU,exp.list.DL,exp.list.FDP,exp.list.GR,exp.list.SPD)
df.germ.list<- data.frame(exp.germ.list)
df.germ.list$id <- c(rep("CDU", 1000), rep("Die Linke", 1000), rep("FDP",1000), rep("Greens",1000),rep("SPD",1000))

pdf(file = "WholeListMPs")
ggplot(df.germ.list, aes(x = exp.germ.list, fill = id)) + 
  geom_density(alpha = 0.4) +
  guides(fill = guide_legend(title = "All list-MPs from each parties")) +
  xlab("number of speeches") +
  ylab("Density") + 
  theme_bw()
dev.off()



#The UK: majoritarian system
library(MASS)
pois.uk<- glm(number_speeches~ideological_distance+party_leader+conservative_MP, data = UKdata, 
              family = "poisson")

negbin.uk<-glm.nb(number_speeches~ideological_distance+party_leader+conservative_MP, data = UKdata, 
                  control = glm.control(maxit = 100))

dispersion.uk<-2*(logLik(negbin.uk)-logLik(pois.uk))
dispersion.uk > qchisq(0.95, df=1) #TRUE
#For the UK, creating a random base model to see whether poisson or negative binomial is better to use
base1.uk<- glm.nb(number_speeches~party_leader+conservative_MP,
                  data = UKdata, control = glm.control(maxit = 100))
base2.uk<-glm(number_speeches~party_leader+conservative_MP,
              data = UKdata, family = "poisson")
dispbase.uk<- 2*(logLik(base1.uk)-logLik(base2.uk))
dispbase.uk > qchisq(0.95, df=1)

For the base of the UK, after the likelihood ratio test, I decided to use the negative binomial regression
model for the different models of the UK.

model1.uk<- glm.nb(number_speeches~ideological_distance+party_leader, data = UKdata, 
                   control = glm.control(maxit = 100))

model2.uk<-glm.nb(number_speeches~ideological_distance+conservative_MP, data=UKdata, 
                  control=glm.control(maxit=100))

model3.uk<-glm.nb(number_speeches~ideological_distance+party_leader+conservative_MP, data = UKdata,
                  control = glm.control(maxit = 100))

summary(model1.uk)
summary(model2.uk)
summary(model3.uk)
library(stargazer)
stargazer(list(model1.uk,model2.uk,model3.uk),out = "pois.negbin.tex",
          title="Negative Binomial Regressions for number of speeches and ideological distance in the UK Parliament",
          notes="Missing Values in the dataset are replaced by the median of the collumn with missing values",
          intercept.bottom=TRUE,
          covariate.labels=c("Ideological Distance","Party Leader","ConservativeMP","Constant"))

anova(model1.uk,model2.uk,model3.uk)
Model 3 is the best model to use because its loglik values is bigger in the negative binomial regression

#Simulations and expected values for the UK
nsim <- 1000
gammahat.uk <- coef(model3.uk)
Vhat.uk <- vcov(model3.uk)
S.uk <- mvrnorm(nsim, gammahat.uk, Vhat.uk)
dim(S.uk)
range(UKdata$ideological_distance)

model3.uk<-glm.nb(number_speeches~ideological_distance+party_leader+conservative_MP, data = UKdata,
                  control = glm.control(maxit = 100))


ideological.distance.sim.uk <- seq(0,6.0, length.out = 100)
scenario2 <- cbind(1, ideological.distance.sim.uk, 0, 0)#not conservative
scenario4<- cbind(1, ideological.distance.sim.uk,0,1)#conservative


Xbeta2 <- S.uk %*% t(scenario2)
lambda2 <- exp(Xbeta2)
p.mean.uk <- apply(lambda2, 2, mean)
p.qu.uk <- t(apply(lambda2, 2, quantile, prob = c(0.025, 0.975)))

Xbeta4<- S.uk %*% t(scenario4)
lambda4<- exp(Xbeta4)
p.mean.cons<-apply(lambda4,2,mean)
p.qu.cons<- t(apply(lambda4,2,quantile, prob=c(0.025, 0.975)))


#For Not CONSERVATIVE
pdf(file = "UK lab simulation")
plot(ideological.distance.sim.uk, p.mean.uk, type="n",
     ylim = c(100,350),
     xlim = c(0,6),
     ylab = "legislative speeches",
     xlab = "ideological distance of non-Conservative MPs",
     bty = "n",
     main = "Effect of Ideological Distance on Number of Speech in the UK",
     las = 1)

polygon(c(rev(ideological.distance.sim.uk),ideological.distance.sim.uk), c(rev(p.qu.uk[,2]), p.qu.uk[,1]),
        col = "gray80",
        border = NA)
lines(ideological.distance.sim.uk, p.mean.uk, lwd = 2)
lines(ideological.distance.sim.uk, p.qu.uk[, 1], lty = "dashed", col = "gray20")
lines(ideological.distance.sim.uk, p.qu.uk[, 2], lty = "dashed", col = "gray20")
dev.off()

#FOR CONSERVATIVE
pdf(file = "UK cons simulation")
plot(ideological.distance.sim.uk, p.mean.cons, type="n",
     ylim = c(100,350),
     xlim = c(0,6),
     ylab = "legislative speeches",
     xlab = "ideological distance of Conservative MPs",
     bty = "n",
     main = "Effect of Ideological Distance on Number of Speeches in the UK",
     las = 1)

polygon(c(rev(ideological.distance.sim.uk),ideological.distance.sim.uk), c(rev(p.qu.cons[,2]), p.qu.cons[,1]),
        col = "gray80",
        border = NA)
lines(ideological.distance.sim.uk, p.mean.cons, lwd = 2)
lines(ideological.distance.sim.uk, p.qu.cons[, 1], lty = "dashed", col = "gray20")
lines(ideological.distance.sim.uk, p.qu.cons[, 2], lty = "dashed", col = "gray20")
dev.off()

We can see the same pattern as given by the hypotheses after plotted version of simulations and adding
the uncertainties of the models. In the majoritarian case, which is the UK, on average as the ideological
distance gets increase in the UK, the average number of speech is also increasing as well. 

#GGplot for the UK model3
model3.uk<-glm.nb(number_speeches~ideological_distance+party_leader+conservative_MP, data = UKdata,
                  control = glm.control(maxit = 100))
summary(model3.uk)
theta.uk<- model3.uk$theta

scenario.CONS<- cbind(1, mean(UKdata$ideological_distance), 0, 1) #Conservative party
scenario.LAB<- cbind(1, mean(UKdata$ideological_distance), 0, 0) #Non Conservative Party

CONSbeta <- S.uk %*% t(scenario.CONS)
LPbeta <- S.uk %*% t(scenario.LAB)

lambdaCONS <- exp(CONSbeta)
lambdaLAB <- exp(LPbeta)

exp.CONS <- sapply(lambdaCONS, function(x) mean(rnbinom(1000, size = theta.uk, mu = x))) 
exp.LAB <- sapply(lambdaLAB, function(x) mean(rnbinom(1000, size = theta.uk, mu = x)))

exp.values.uk <- c(exp.CONS, exp.LAB)
df.uk<- data.frame(exp.values.uk)
df.uk$id <- c(rep("Conservative", 1000), rep("Non-Conservative", 1000))


pdf(file = "GGplot UK")
ggplot(df.uk, aes(x = exp.values.uk, fill = id)) + 
  geom_density(alpha = 0.4) +
  guides(fill = guide_legend(title = "Conservative vs Non-Conservative MPs")) +
  xlab("number of speeches") +
  ylab("Density") + 
  theme_bw() 
dev.off()

#Taking the first differences between Conservative and Non-conservative MPs 
fd<- exp.CONS-exp.LAB
range(fd)
mean(fd)
quants.mean.fun <- function(x){
  c(quants=quantile(x,probs = c(0.025,0.975)), mean=mean(x))
}
quants.fd <- apply(as.matrix(fd), 2, quants.mean.fun)
library(MASS)
library(foreign)
install.packages("ggplot2")
library(ggplot2)
pdf(file = "FD the uk")
plot (density(fd),xlim = c(-13,180), main="First Differences", 
      xlab="First differences between Conservative and Non-Conservative MPs",
      ylab="density of the first differences")
abline(v=mean(fd))
abline(v = c(quants.fd[, 1]), lty = 2, col = "black")
dev.off()



