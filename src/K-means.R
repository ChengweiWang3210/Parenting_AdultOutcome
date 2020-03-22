
# kmeans -----
tmp <- c()

dt_clst <- dta[,piv[c(1:3,13:16)]]
dt_clst[dt_clst < 0] <- NA
findNA <- t(apply(dt_clst,1,is.na))
li <- apply(findNA, 1, sum)
summary(li)
idx <- which(li == 0)
dt_clst <- dt_clst[idx,]

for (i in 1:7) {tmp <- c(tmp,kmeans(dt_clst,i)$tot.withinss)}
plot(x = 1:7, y = tmp)
# 4 seems acceptable

clst <- kmeans(dt_clst,4)
aggregate(dt_clst, by = list(clst$cluster), mean, na.rm = T)

dt <- dta[idx,var]
dt$parenttype <- clst$cluster
dt[dt<=0] <- NA
colnames(dt) <- tolower(str_remove(colnames(dt),'X1|S1|X4'))
dt$p1hhtime <- 6 - dt$p1hhtime
dt$p1nooutsch <- 1-dt$p1nooutsch

m1 <- lm(incomecat ~ (hractivity) + (famincome) +
           as.factor(parenttype), data = dt)
summary(m1)

m1_2 <- update(m1, ~. + hractivity:famincome)
summary(m1_2)

m2 <- update(m1, ~.+ momedu)
summary(m2)

m2_2 <-  update(m2, ~.+ dadedu)
summary(m2_2)

anova(m2,m2_2)

m3 <- update(m2_2, ~.+ as.factor(race) + sex + as.factor(region) + as.factor(parpattern) +
               momemp)
summary(m3)
anova(m2,m3)
anova(m2_2,m3)
m3_2 <- update(m3, ~.+ as.factor(momocc2) + as.factor(momrace))

anova(c,c01)

m3_3 <- update(m3_2, ~.+ as.factor(race) + sex + as.factor(region) + as.factor(parpattern) +
                 dademp + as.factor(dadocc2) + as.factor(dadrace))
summary(m3_3)
anova(m3_2,m3_3)

m4 <- update(m3_3, ~.+famincome:hractivity)
summary(m4)

