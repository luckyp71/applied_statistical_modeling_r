# ------------ Power Test for Detecting Effect
# stat test will not be able to detect the true diff i the sample size is too small
# check the sample size we collect
require(datasets)
require(pwr)

?power.t.test

power.t.test(n=50, delta=1, sd=3, sig.level=0.05, power=NULL, type='one.sample')
# we have only a 64% chance of detecting an effect that size

power.t.test(n=NULL, delta=1, sd=3, sig.level=0.05, power=.8, type='one.sample')
# one sample t test determines whether the sample mean is statistically different from a known or a hypothesized population mean or not.
# we need n=73 sample to detect 80% chance of being able to detect a difference of one unit.


# two sample problems
power.t.test(n=NULL, delta=1, sd=3, sig.level=.05, power=.8)

# how large must a difference be or us to detect it with 80% power
# if we only have 50 subjects per group?

power.t.test(n=50, delta = NULL, sd=3, sig.level=0.05, power=.8)
# difference between samples should be at least 1.7 for the t test to detect the difference


# ANOVA
# one-way ANOVA comparing 4 groups, calculate the 
# sample size needed in each group to obtain a power of
# 0.80, when the effect size is moderate (0.25)
pwr.anova.test(NULL, k=4, f=.25, sig.level = .05, power=.8)

# collect 45 observations per group