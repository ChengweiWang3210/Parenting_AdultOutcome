
# Multiple Linear Regression

a <- lm(incomecat ~ hractivity + momtalkprb + dadtalkprb + p1nooutsch +
          p1ptconfer + p1hwoften + p1fundraise + p1ptomtg + p1hhtime +
          p1volunteer + p1schevent, data = dt)
summary(a)

# + p1arts + p1sports + p1religgrp + p1club + p1academic + p1campms + p1campoth + p1nooutsch

b <- update(a, ~. + famincome  , data = dt)
summary(b)

c <- update(b, ~.+ sex)
summary(c)

d <- update(c, ~.+ as.factor(race) + momedu + dadedu + as.factor(locale) +
              momemp + as.factor(momocc2) + 
              dademp + as.factor(dadocc2))
summary(d)

#sex : 1 male, 2 female

# d1 <- update(c, ~.+ as.factor(race) + sex + as.factor(locale) +
#               momemp + as.factor(momocc2) +
#               dademp + as.factor(dadocc2) +
#               as.factor(parpattern) + as.factor(momrace) +
#                as.factor(dadrace))
# summary(d1)
# + as.factor(parpattern) + momrace, dadrace vif too high
e <- update(d, ~.+ famincome:p1nooutsch)
summary(e)

anova(d,e)

f <- update(e, ~.+ famincome:hractivity)
summary(f)
anova(e,f)