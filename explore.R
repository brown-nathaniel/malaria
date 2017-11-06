library(ggplot2); library(reshape2)
dat <- read.csv("gambiaMissing.csv")
# Y: indicator of whether malaria parasites were found in a blood sample from the child (1 = yes)
# AGE: age of the child, in years
# BEDNET: indicator of whether the child has a net over his or her bed (1 = yes)
# GREEN: a measure of how much greenery is around the child's village, derived from satellite images (units are arbitrary)
# PHC: indicator for the presence of a public health clinic in the child's village (1 = yes)

summary(dat)      #bednet is the only column with NA's
#GREEN only has 5 unique values
NArows <- which(is.na(dat$BEDNET))
dat$miss <- as.numeric(is.na(dat$BEDNET))

nullmod <- glm(miss ~ 1, family="binomial", data=dat)
fullmod <- glm(miss ~ AGE + GREEN + PHC, family="binomial", data=dat) #AGE and PHC appear to be "significant" predictors of missing BEDNET
(anova(nullmod, fullmod, test="Chisq"))
pchisq(fullmod$deviance, fullmod$df.residual,lower = FALSE)

arm::binnedplot(nullmod$fitted.values, nullmod$residuals)
arm::binnedplot(fullmod$fitted.values, fullmod$residuals)

hist(dat$GREEN)

group_bednet <- function(predictor = NA){
  bednet_group <- dat %>% 
                group_by_(predictor) %>% 
                summarize(BEDNET1 = mean(BEDNET==1 & !is.na(BEDNET)), 
                          BEDNET0 = mean(BEDNET==0 & !is.na(BEDNET)), 
                          BEDNETNA = mean(is.na(BEDNET))) %>%
                melt(id=predictor)
  return(bednet_group)
}

ggplot(data=bednet_age, aes(x=AGE, y=value, fill=variable)) + geom_col(position="dodge")#+ geom_bar(aes(fill=as.factor(BEDNET)), position="dodge")

ggplot(data=dat, aes(x=as.factor(GREEN))) + geom_bar(aes(fill=as.factor(BEDNET)), position="dodge")

ggplot(data=dat, aes(x=as.factor(PHC))) + geom_bar(aes(fill=as.factor(BEDNET)), position="dodge")

ggplot(data=dat, aes(x=as.factor(Y))) + geom_bar(aes(fill=as.factor(BEDNET)), position="dodge")
