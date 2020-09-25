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
      scale_x_date("", date_labels="%b %Y", breaks="4 years") +
      theme_bw() +
      theme(plot.title = element_text(hjust = 0.5)) +
      ggtitle("S&P 500"))
filename <- "s&p500_close.png"
ggsave(filename, plot=p, width=10, height=8)

p <- (ggplot(df, aes(x=date, y=log(close))) +
      geom_line() +
      ylab("log closing price") +
      scale_x_date("", date_labels="%b %Y", breaks="4 years") +
      theme_bw() +
      theme(plot.title = element_text(hjust = 0.5)) +
      ggtitle("S&P 500"))
filename <- "s&p500_log_close.png"
ggsave(filename, plot=p, width=10, height=8)

p <- (ggplot(df, aes(x=date, y=log_return)) +
      geom_point(alpha=0.5) +
      ylab("log returns (close-to-close)") +
      scale_x_date("", date_labels="%b %Y", breaks="4 years") +
      theme_bw() +
      theme(plot.title = element_text(hjust = 0.5)) +
      ggtitle("S&P 500"))
filename <- "s&p500_log_return.png"
ggsave(filename, plot=p, width=10, height=8)

cutoff_date <- as.Date("2010-01-01")
train <- df[df$date < cutoff_date, ]
test <- df[df$date >= cutoff_date, ]

## Let's fit a simple model to the training set
## and evaluate its log likelihood on the test set
train_mean <- mean(train$log_return, na.rm=TRUE)
train_var <- var(train$log_return, na.rm=TRUE)

## TODO Try some other vanilla models: iid student's t, mixture of normals, HMM, etc etc
log_likelihood_iid_normal <- mean(dnorm(test$log_return, mean=train_mean, sd=sqrt(train_var), log=TRUE))
