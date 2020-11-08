library(fpp2)
library(gridExtra)

grid.arrange(autoplot(goog200,series="Google stock value"),
             autoplot(diff(goog200),series="Google stock daily change"))

ggAcf(goog200)
ggAcf(diff(goog200))

sd(goog200)
set.seed(12345)
tmp <- c(400,rnorm(199,0,sd(goog200)/sqrt(200)))
fake.goog200 <- ts(cumsum(tmp))
autoplot(goog200,series="Real") + autolayer(fake.goog200,series="Fake data")

set.seed(12345)
autoplot(ts(cumsum(rnorm(1000))))

# Stock market: noise plus unexpected "jump" events
y <- diff(goog200)
y2 <- y[-165]
sd(y2)
jump <- y[165]/sd(y2)
autoplot(goog200,series="Google stock prices",col=1) +
autolayer(ts(c(goog200[165],goog200[166]),start=165),series=sprintf("A %.0f sigma event!",jump))

