# This video:
# - More on r base plotting (and the formula syntax)
# - The "lattice" library
#
# Next video: advanced plotting with "ggplot"
str(df)
df$l.votes <- log10(df$votes)
plot(df$l.votes)
# formula syntax
# Y ~ X
plot(df$l.votes ~ df$party)
plot(df$l.votes ~ df$state_abbreviation)

library(lattice)

histogram( ~ l.votes | party+sex,df)
histogram( ~ l.votes | state_abbreviation,df)

z <- rnorm(nrow(df))
plot(z)
hist(z)
df$z <- z
str(df)

xyplot(l.votes~z | sex + party,df)
