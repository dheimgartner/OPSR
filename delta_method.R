## https://stats.oarc.ucla.edu/r/faq/how-can-i-estimate-the-standard-error-of-transformed-regression-parameters-in-r-using-the-delta-method/

d <- read.csv("https://stats.idre.ucla.edu/stat/data/hsbdemo.csv")
d$honors <- factor(d$honors, levels=c("not enrolled", "enrolled"))
m3 <- glm(honors ~ female + math + read, data=d, family=binomial)
summary(m3)
b2 <- coef(m3)[3]
vb2 <- vcov(m3)[3,3]
deltamethod(~ exp(x1), b2, vb2)
deltamethod(~exp(pnorm(x1)), b2, vb2)
