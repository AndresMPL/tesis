
library(haven)
hsbdemo <- read_sav("E:/hsbdemo.sav")
hsb<-hsbdemo # Get a new copy of data
summary(hsb)

library(jmv)


descriptives(hsb, vars = vars(ses, prog, math, science), freq = TRUE)


# To see the crosstable, we need CrossTable function from gmodels package
library(gmodels)
# Build a crosstable between admit and rank
CrossTable(hsb$ses, hsb$prog)

# Load the multinom package
library(nnet)
# Since we are going to use Academic as the reference group, we need relevel the group.
hsb$prog2 <- relevel(as.factor(hsb$prog), ref = 2)
hsb$ses <- as.factor(hsb$ses)
levels(hsb$prog2)

# Give the names to each level
levels(hsb$prog2) <- c("academic","general","vocational")
# Run a "only intercept" model
OIM <- multinom(prog2 ~ 1, data = hsb)

summary(OIM)

# Run a multinomial model
multi_mo <- multinom(prog2 ~ ses + math + science + math*science, data = hsb,model=TRUE)


summary(multi_mo)

# Check the Z-score for the model (wald Z)
z <- summary(multi_mo)$coefficients/summary(multi_mo)$standard.errors
z

# 2-tailed z test
p <- (1 - pnorm(abs(z), 0, 1)) * 2
p

detach("package:jmv", unload=TRUE)
# Compare the our test model with the "Only intercept" model
anova(OIM,multi_mo)

head(multi_mo$fitted.values,30)

# We can get the predicted result by use predict function
head(predict(multi_mo),30)

# Test the goodness of fit
chisq.test(hsb$prog2,predict(multi_mo))

library("DescTools")

PseudoR2(multi_mo, which = c ("CoxSnell", "Nagelkerke", "McFadden"))

library(lmtest)


lrtest(multi_mo, "ses") # Chi-Square=12.922,p=0.01166*

lrtest(multi_mo, "math") # Chi-Square=10.613,p=0.004959*

lrtest(multi_mo, "science") # Chi-Square=5.3874,p=0.06763

lrtest(multi_mo, "math:science") # Chi-Square=5.249,p=0.072

summary(multi_mo)

z <- summary(multi_mo)$coefficients/summary(multi_mo)$standard.errors
z

p <- (1 - pnorm(abs(z), 0, 1))*2
p

exp(coef(multi_mo))

library(summarytools)

ctable <- table(hsb$prog2,predict(multi_mo))
ctable