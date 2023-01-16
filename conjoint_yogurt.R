rm(list=ls())
require(mlogit)
setwd("C:\\Users\\Avet\\Desktop\\Git\\R\\Conjoint analysis") ## directory


yogurt = read.csv("conjoint_yogurt.csv")

attach(yogurt)

head(yogurt)

yogurt$size = as.factor(yogurt$size)
yogurt$price = as.factor(yogurt$price)

# cross tabulations
xtabs(choice~ diet, data= yogurt)
xtabs (choice ~ shp, data=yogurt)
xtabs (choice ~ price, data=yogurt)


# add a column with unique question numbers, as needed in mlogit 1.1+
yogurt$chid <- rep(1:(nrow(yogurt)/3), each=3)
# shape the data for mlogit
yogurt.mlogit<- dfidx(yogurt, choice="choice", idx=list(c("chid", "resp.id"), "alt" ))


attach(yogurt.mlogit)

# without intercept, assuming that the position of the question in the survey doesn't matter for respondents 
m1 <- mlogit(choice ~ 0  + size + shp + flav + price, data = yogurt.mlogit)
summary(m1)
round(exp(coef(m1)),3)

# include intercept and conduct 'lrtest' to bring an argument in favor of assumption provided above 
m2 <- mlogit(choice ~  size +shp + flav +price , data = yogurt.mlogit)
summary(m2)
lrtest(m1, m2)


m3 <- mlogit(choice ~   0+ size +shp + flav +as.numeric(price) , data = yogurt.mlogit)
summary(m3)
lrtest(m1, m3)


coef(m3)
round(coef(m3)[-6]/(-coef(m3)["as.numeric(price)"]/10),2)

