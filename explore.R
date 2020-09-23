library(ggplot2)
library(quantmod)

sp500 <- getSymbols("^GSPC", auto.assign=FALSE, from="1970-01-01")

df <- data.frame(date=as.Date(index(sp500)),
                 volume=as.numeric(sp500$GSPC.Volume),
                 close=as.numeric(sp500$GSPC.Close))

df$log_return <- c(NA, diff(log(df$close)))

p <- (ggplot(df, aes(x=date, y=close)) +
      geom_line() +
      ylab("closing price") +
      scale_x_date("", date_labels="%b %Y") +
      theme_bw() +
      theme(plot.title = element_text(hjust = 0.5)) +
      ggtitle("S&P 500"))
p

p <- (ggplot(df, aes(x=date, y=log_return)) +
      geom_point(alpha=0.5) +
      ylab("log returns (close-to-close)") +
      theme_bw() +
      theme(plot.title = element_text(hjust = 0.5)) +
      ggtitle("S&P 500"))
p


