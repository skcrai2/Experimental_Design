#To investigate whether or
#not time is a factor in how high bread dough rises, you prepare a large batch
#of bread dough. You divide the batch into rt sub-batches (r is the number of
#replicates and t is the number of treatments) and randomly assign r of the
#sub-batches to each of the t treatments. Based on prior experience with this
#recipe, you decide to use time factor levels of 35 minutes, 40 minutes, and
#45 minutes, so that t = 3. You decide to do 4 replicates. This requires 12
#sub-batches.

setwd("C:\\pathtowhereyouwanttosaveyourfile")
set.seed(7638)
fac.levels <- factor(rep(c(35, 40, 45), each = 4))
fac <- sample(fac.levels, 12) #sampling without replacement
loaf.num <- 1:12
plan <- data.frame(loaf.number=loaf.num, rise.time=fac)
plan$height <- rep("", 12)
write.csv(plan, file="Plan.csv", row.names=F)
plan

#Suppose if there is a difference of 3 inches in the
#response (rise height) between any two treatments, and we want to detect this.

rmin <- 2
rmax <- 10
alpha <- rep(0.05, rmax - rmin + 1)
alpha
sigma <- sqrt(2.1) # This is a guess based on preivous experience.
nlev <- 3
r <- rmin:rmax
r
D <- 3 # This is the difference you’d like to detect.
lambda <- (r*D^2)/(2 * sigma^2)
lambda #Note lambda depends on r.
Fcrit <- qf(1-alpha, df1=nlev-1, df2=nlev*(r-1))
Fcrit
1-pf(Fcrit, df1=nlev-1, df2=nlev*(r-1), ncp = lambda)
# Or just use the Fpower1 function from the daewr library:
library(daewr)
power <- Fpower1(alpha, nlev, r, D, sigma)
power

#so 6 reps gets power of over 80%, and 8 reps get the power over 90%. 
#Expect to require more reps if we want to detect a smaller difference.

# To detect difference of 2 inches:
rmin <- 2
rmax <- 20 # Must increase because will require more reps.
alpha <- rep(0.05, rmax - rmin + 1)
alpha
sigma <- sqrt(2.1) # This is a guess based on preivous experience.
nlev <- 3
r <- rmin:rmax
r
D <- 2 # This is the new difference you’d like to detect.
lambda <- (r*D^2)/(2 * sigma^2)
lambda #Note lambda depends on r.
Fcrit <- qf(1-alpha, df1=nlev-1, df2=nlev*(r-1))
Fcrit
1-pf(Fcrit, df1=nlev-1, df2=nlev*(r-1), ncp = lambda)
# Or just use the Fpower1 function from the daewr library:
library(daewr)
power <- Fpower1(alpha, nlev, r, D, sigma)
power

#If we want to detect a difference of 2 inches between the mean response for
#any two treatments, we would need 12 for power over 80% and 15 for the
#power to exceed 90%.

library(daewr)
mod0 <- lm(height ~ time, data= bread)
summary(mod0)
