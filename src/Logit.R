
#--------------------------logit-----------------------
dt[,isMob:=ifelse(incomecat - famincome > 0, 1, 
                  ifelse(incomecat - famincome == 0, 0, 
                         ifelse(incomecat - famincome < 0 , 0 , NA)))]

summary(dt$isMob)

m1 <- glm(isMob ~ hractivity, data = dt, family = 'binomial')
summary(m1)

m2 <- update(m1, ~.+famincome)
summary(m2)

m3 <- update(m2, ~. + momedu + as.factor(race) + sex + as.factor(region) + 
               momemp + as.factor(momocc2) + as.factor(momrace) + as.factor(parpattern))
summary(m3)
