library(quantmod)

indf_data <- getSymbols(Symbols = "INDF.JK", src = "yahoo", from = Sys.Date() - 1953, 
                        to = Sys.Date(), auto.assign = FALSE)

indf_data <- Cl(indf_data)

ibex <- new.env()
getSymbols("^IBEX", env = ibex, src = "yahoo", from = as.Date("2010-01-04"), to = as.Date("2015-11-30"))
ibex <- ibex$IBEX
ibex <- ibex$IBEX.Adjusted
plot(ibex)
decompose(ibex)
auto.arima(ibex,trace=TRUE)
checkresiduals(Arima(ibex,order=c(0,1,0)))
checkresiduals(Arima(ibex,order=c(0,1,2)))
