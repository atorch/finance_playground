library(ggplot2)
library(mixtools)
library(quantmod)

sp500 <- getSymbols("^GSPC", auto.assign=FALSE, from="1970-01-01")

df <- data.frame(date=as.Date(index(sp500)),
                 volume=as.numeric(sp500$GSPC.Volume),
                 close=as.numeric(sp500$GSPC.Close))

df$log_return <- c(NA, diff(log(df$close)))

breaks = seq.Date(as.Date("1970-01-01"), as.Date("2020-01-01"), by="5 years")

p <- (ggplot(df, aes(x=date, y=close)) +
      geom_line() +
      ylab("close") +
      scale_x_date("", date_labels="%b %Y", breaks=breaks) +
      theme_bw() +
      theme(plot.title = element_text(hjust = 0.5)) +
      ggtitle("S&P 500"))
filename <- "s&p500_close.png"
ggsave(filename, plot=p, width=10, height=8)

p <- (ggplot(df, aes(x=date, y=log(close))) +
      geom_line() +
      ylab("log close") +
      scale_x_date("", date_labels="%b %Y", breaks=breaks) +
      theme_bw() +
      theme(plot.title = element_text(hjust = 0.5)) +
      ggtitle("S&P 500"))
filename <- "s&p500_log_close.png"
ggsave(filename, plot=p, width=10, height=8)

p <- (ggplot(df, aes(x=date, y=log_return)) +
      geom_point(alpha=0.5) +
      ylab("log returns (close-to-close)") +
      scale_x_date("", date_labels="%b %Y", breaks=breaks) +
      theme_bw() +
      theme(plot.title = element_text(hjust = 0.5)) +
      ggtitle("S&P 500"))
filename <- "s&p500_log_return.png"
ggsave(filename, plot=p, width=10, height=8)

cutoff_date <- as.Date("2010-01-01")
train <- df[df$date < cutoff_date, ]
test <- df[df$date >= cutoff_date, ]

## Let's fit some simple models to the training set
## and evaluate their log likelihoods on the test set
train_mean <- mean(train$log_return, na.rm=TRUE)
train_var <- var(train$log_return, na.rm=TRUE)

normal_mixture_two_components <- normalmixEM(tail(train$log_return, nrow(train) - 1), k=2)
normal_mixture_three_components <- normalmixEM(tail(train$log_return, nrow(train) - 1), k=3)

## TODO Try some other vanilla models: iid student's t, mixture of normals, HMM, etc etc
log_likelihood_iid_normal <- mean(dnorm(test$log_return, mean=train_mean, sd=sqrt(train_var), log=TRUE))

mixture_log_likelihood <- function(x, mixture) {
    return(log(sum(mixture$lambda * dnorm(x, mean=mixture$mu, sd=mixture$sigma))))
}

log_likelihood_mixture_two_components <- mean(sapply(test$log_return, mixture_log_likelihood, mixture=normal_mixture_two_components))
log_likelihood_mixture_three_components <- mean(sapply(test$log_return, mixture_log_likelihood, mixture=normal_mixture_three_components))

