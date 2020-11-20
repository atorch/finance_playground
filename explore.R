library(ggplot2)
library(mixtools)
library(quantmod)

starting_date <- as.Date("1970-01-01")

sp500 <- getSymbols("^GSPC", auto.assign=FALSE, from=starting_date)

df <- data.frame(date=as.Date(index(sp500)),
                 volume=as.numeric(sp500$GSPC.Volume),
                 close=as.numeric(sp500$GSPC.Close))

df$log_return <- c(NA, diff(log(df$close)))

breaks = seq.Date(starting_date, as.Date("2020-01-01"), by="5 years")

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
      geom_point(alpha=0.25) +
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

## TODO Try some other vanilla models: iid student's t, HMM, etc etc
log_likelihood_iid_normal <- mean(dnorm(test$log_return, mean=train_mean, sd=sqrt(train_var), log=TRUE))

mixture_density <- function(x, mixture) {
    ## The input x is a scalar, and the mixture is an object returned by normalmixEM
    return(sum(mixture$lambda * dnorm(x, mean=mixture$mu, sd=mixture$sigma)))
}

mixture_log_likelihood <- function(x, mixture) {
    return(log(mixture_density(x, mixture)))
}

get_mixture_test_set_log_likelihood <- function(mixture_model) {
    return(mean(sapply(test$log_return, mixture_log_likelihood, mixture=mixture_model)))
}

mixture_n_components <- seq(2, 5)
mixture_models <- lapply(mixture_n_components, function(n_components) normalmixEM(tail(train$log_return, nrow(train) - 1), maxit=10000, k=n_components))
mixture_likelihoods <- sapply(mixture_models, get_mixture_test_set_log_likelihood)

log_likelihoods <- rbind(data.frame(model="i.i.d. Normal", likelihood=log_likelihood_iid_normal),
                         data.frame(model=sprintf("i.i.d. Normal Mixture (%s components)", mixture_n_components), likelihood=mixture_likelihoods))

p <- (ggplot(log_likelihoods, aes(y=model, x=likelihood)) +
      geom_point() +
      theme_bw() +
      xlab(sprintf("test set (%s to present) log likelihood", cutoff_date)) +
      ylab("") +
      theme(plot.title = element_text(hjust = 0.5)) +
      ggtitle("Test Set Log Likelihoods"))
filename <- "test_set_log_likelihood.png"
ggsave(filename, plot=p, width=10, height=8)

mixture_density_vectorized <- Vectorize(mixture_density, vectorize.args="x")

## Histogram of test set log returns alongside model densities estimated from the _training_ set
## TODO Needs a legend for the functions
p <- (ggplot(test, aes(x=log_return)) +
      geom_histogram(aes(y=..density..), binwidth=0.001, color="grey", fill="white") +
      geom_function(fun=dnorm, color="#5ab4ac", n=1000, args=list(mean=train_mean, sd=sqrt(train_var)), linetype=2) +
      geom_function(fun=mixture_density_vectorized, color="black", n=1000, args=list(mixture=mixture_models[[1]])) +
      geom_function(fun=mixture_density_vectorized, color="#d7191c", n=1000, args=list(mixture=mixture_models[[2]])) +
      geom_function(fun=mixture_density_vectorized, color="#a6611a", n=1000, args=list(mixture=mixture_models[[3]])) +
      theme_bw() +
      xlab("log return") +
      ylab("probability density") +
      theme(plot.title = element_text(hjust = 0.5)) +
      ggtitle("Test Set Log Return Histogram & Model Densities"))
filename <- "test_set_log_returns_histogram_and_model_densities.png"
ggsave(filename, plot=p, width=10, height=8)
