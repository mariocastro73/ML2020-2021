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
