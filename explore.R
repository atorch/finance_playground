library(ggplot2)
library(mixtools)
library(quantmod)

source("hmm_gaussian_functions.R")

starting_date <- as.Date("1970-01-01")

sp500 <- getSymbols("^GSPC", auto.assign=FALSE, from=starting_date)

df <- data.frame(date=as.Date(index(sp500)),
                 volume=as.numeric(sp500$GSPC.Volume),
                 close=as.numeric(sp500$GSPC.Close))

df$log_return <- c(NA, diff(log(df$close)))

breaks = seq.Date(starting_date, as.Date("2026-01-01"), by="5 years")

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

students_t_density <- function(x, params) {
    params <- as.list(params)
    return(dt((x - params$location) / params$scale, df=params$degrees_freedom) / params$scale)
}

students_t_log_density <- function(x, params) {
    params <- as.list(params)
    return(dt((x - params$location) / params$scale, df=params$degrees_freedom, log=TRUE) - log(params$scale))
}

## TODO Estimate by EM?  Multiple starting values for optimization?
log_likelihood_students_t <- function(params, dataframe=train) {
    ## Log likelihood for iid Student's t
    ## (scaled and shifted as in https://www.mathworks.com/help/stats/t-location-scale-distribution.html)
    return(mean(students_t_log_density(dataframe$log_return, params), na.rm=TRUE))
}

optim_results <- optim(par=c(location=0, scale=0.1, degrees_freedom=10),
                       lower=c(-Inf, 0.001, 4),
                       fn=log_likelihood_students_t, method="L-BFGS-B", control=list(fnscale=-1, trace=1, maxit=100), hessian=TRUE)

students_t_params <- optim_results$par

mixture_n_components <- seq(2, 5)
mixture_models <- lapply(mixture_n_components, function(n_components) normalmixEM(tail(train$log_return, nrow(train) - 1), maxit=10000, k=n_components))
mixture_likelihoods <- sapply(mixture_models, get_mixture_test_set_log_likelihood)

## Regime-switching HMM: discrete hidden state, log returns ~ N(m(X_k), v(X_k)) conditional on the
## regime X_k. Follows sec. 6.3 of hmm080728.pdf (Fig. 6.2, GE stock example): sweep model order
## (number of regimes) d, plot the scaled log-likelihood n^-1 log L*_n(d) on the training set and
## look for the 'corner'.
train_log_returns <- train$log_return[-1]  # Drop the leading NA (no lagged price for the very first observation)
train_dates_hmm <- train$date[-1]
train_close_hmm <- train$close[-1]
n_train_obs <- length(train_log_returns)

hmm_max_order <- 5
hmm_fits <- lapply(seq_len(hmm_max_order), function(d) {
    message("Fitting Gaussian HMM with ", d, " regime(s)")
    fit_gaussian_hmm(train_log_returns, n_components=d, n_random_starts=5, max_iter=50, epsilon=0.001)
})

## Sanity check: with a single hidden state there is no regime dynamics at all (P is the 1x1
## identity), so a 1-regime HMM is mathematically just the i.i.d. Gaussian MLE -- it should recover
## exactly the same mean/variance as train_mean/train_var above (up to train_var's n-1, rather than
## the MLE's n, denominator).
train_var_mle <- train_var * (n_train_obs - 1) / n_train_obs
stopifnot(isTRUE(all.equal(hmm_fits[[1]]$m, train_mean, tolerance=1e-6, check.attributes=FALSE)))
stopifnot(isTRUE(all.equal(hmm_fits[[1]]$v, train_var_mle, tolerance=1e-6, check.attributes=FALSE)))
message("Sanity check passed: 1-regime HMM MLE (m=", signif(hmm_fits[[1]]$m, 6), ", v=", signif(hmm_fits[[1]]$v, 6),
        ") matches the i.i.d. Gaussian MLE on the training set (mean=", signif(train_mean, 6),
        ", population variance=", signif(train_var_mle, 6), ")")

hmm_scaled_loglik <- data.frame(n_components=seq_len(hmm_max_order),
                                scaled_log_likelihood=sapply(hmm_fits, function(fit) tail(fit$loglik, 1) / n_train_obs))

p <- (ggplot(hmm_scaled_loglik, aes(x=n_components, y=scaled_log_likelihood)) +
      geom_line() +
      geom_point() +
      scale_x_continuous(breaks=seq_len(hmm_max_order)) +
      theme_bw() +
      xlab("model order (number of regimes)") +
      ylab("scaled log-likelihood (per observation)") +
      theme(plot.title = element_text(hjust = 0.5)) +
      ggtitle("HMM Model Order Selection (Scaled Log-Likelihood)"))
filename <- "hmm_scaled_log_likelihood.png"
ggsave(filename, plot=p, width=10, height=6)

## Test set log likelihood for each HMM order, on the same per-observation average log density
## footing as the i.i.d. models below, so they can be compared head to head. Unlike the i.i.d.
## models, though, an HMM's likelihood for a sequence isn't a sum of independent per-point
## densities: Pr(y_1, ..., y_T | theta) factors via the forward algorithm as
## prod_k Pr(y_k | y_1, ..., y_{k-1}, theta). theta (P, m, v) comes only from EM on the *training*
## set -- test data is never used to fit a single parameter. What we score is "HMM, sequential
## filtering": filter forward through the *entire* series from its first observation (~1970) all
## the way through the test set, using the fixed, train-only theta_hat throughout, and average the
## log predictive-density contributions that fall in the test period. Equivalently, at every test
## point K this is Pr(Y_{K+1} | Y_K, Y_{K-1}, ..., Y_1; theta_hat) -- the filtered belief carries
## forward all information from the beginning of the series, exactly mirroring how you'd actually
## deploy this model sequentially, with no leakage (theta is fixed, and the belief at each point
## only depends on *past* observations).
##
## Two variants we considered and dropped as not worth the clutter: (a) resetting the state belief
## to P_hat's stationary distribution at the test boundary instead of carrying it forward -- but for
## an ergodic chain this is mathematically guaranteed to converge back to sequential filtering after
## a short transient (empirically ~15-20 days here) once both are updating on the same real data, so
## it isn't actually a different question, just a noisier version of the same one; (b) scoring every
## test point against the fixed long-run marginal (stationary) mixture density, never updating the
## regime belief at all -- this *is* a meaningfully different (non-adaptive) benchmark, but it's an
## i.i.d. mixture model in every way that matters, redundant with the i.i.d. Gaussian Mixture rows
## already in this plot.
full_log_returns <- df$log_return[-1]  # Drop the leading NA; spans train and test
full_dates <- df$date[-1]
test_idx_in_full <- full_dates >= cutoff_date
stopifnot(sum(test_idx_in_full) == nrow(test))

get_hmm_test_set_scaled_log_likelihood <- function(fit) {
    ## fit$mu is left as-is (the stationary distribution of the fitted P, used as the initial-state
    ## distribution for the very first observation of the training set): the forward pass starts
    ## there and filters continuously through the full history, so by the time it reaches the test
    ## set ~40 years later, the belief is driven by the intervening data, not by this initial
    ## condition.
    full_c <- baum_welch_gaussian_time_homogeneous(list(y=full_log_returns), fit)$c
    return(mean(log(full_c[test_idx_in_full])))
}
hmm_test_likelihoods <- sapply(hmm_fits, get_hmm_test_set_scaled_log_likelihood)

## Sanity check: with a single regime there's no hidden-state dynamics to exploit (the predictive
## density is the same fixed Gaussian at every k), so this must land very close to the i.i.d.
## Gaussian test log likelihood above (not bit-for-bit identical, since train_var above uses the
## n-1, rather than the MLE's n, denominator, but the gap should be negligible at this sample size)
message("1-regime HMM test set scaled log likelihood: ", signif(hmm_test_likelihoods[1], 6),
        ", vs i.i.d. Gaussian: ", signif(log_likelihood_iid_normal, 6))
stopifnot(isTRUE(all.equal(hmm_test_likelihoods[1], log_likelihood_iid_normal, tolerance=1e-3)))

mixture_model_labels <- sprintf("i.i.d. Gaussian Mixture (%s components)", mixture_n_components)
hmm_model_labels <- sprintf("HMM (%s regime%s, sequential filtering)", seq_len(hmm_max_order), ifelse(seq_len(hmm_max_order) == 1, "", "s"))
log_likelihoods <- rbind(data.frame(model="i.i.d. Gaussian", likelihood=log_likelihood_iid_normal),
                         data.frame(model=mixture_model_labels, likelihood=mixture_likelihoods),
                         data.frame(model="i.i.d. Student's t", likelihood=log_likelihood_students_t(students_t_params, dataframe=test)),
                         data.frame(model=hmm_model_labels, likelihood=hmm_test_likelihoods))

## Top-to-bottom display order: the 1-regime HMM sits directly below the i.i.d. Gaussian it should
## match exactly (visual sanity check), then HMM orders increase going down. ggplot draws the first
## factor level at the *bottom* of a discrete y axis, hence the rev().
model_display_order <- c("i.i.d. Student's t", rev(mixture_model_labels), "i.i.d. Gaussian", hmm_model_labels)
stopifnot(setequal(model_display_order, log_likelihoods$model))
log_likelihoods$model <- factor(log_likelihoods$model, levels=rev(model_display_order))

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
mixture_log_density_vectorized <- Vectorize(mixture_log_likelihood, vectorize.args="x")

## Histogram of test set log returns alongside model densities estimated from the _training_ set
mixture_names <- sprintf("Gaussian Mixture (%s components)", mixture_n_components)
color_values <- c("#5ab4ac", "black", "#ca0020", "#bcbddc")
linetype_values <- c(2, 1, 1, 3)
names(color_values) <- names(linetype_values) <- c("Gaussian", mixture_names[1], mixture_names[3], "Student's t")
p <- (ggplot(test, aes(x=log_return)) +
      geom_histogram(aes(y=..density..), binwidth=0.001, color="grey", fill="white") +
      geom_function(aes(colour="Gaussian", linetype="Gaussian"),
                    fun=dnorm, n=1000, args=list(mean=train_mean, sd=sqrt(train_var))) +
      geom_function(aes(colour=mixture_names[1], linetype=mixture_names[1]),
                    fun=mixture_density_vectorized, n=1000, args=list(mixture=mixture_models[[1]])) +
      geom_function(aes(colour=mixture_names[3], linetype=mixture_names[3]),
                    fun=mixture_density_vectorized, n=1000, args=list(mixture=mixture_models[[3]])) +
      geom_function(aes(color="Student's t", linetype="Student's t"),
                    fun=students_t_density, n=1000, args=list(params=students_t_params)) +
      scale_color_manual("Model", values=color_values) +
      scale_linetype_manual("Model", values=linetype_values) +
      theme_bw() +
      xlab("log return") +
      ylab("probability density") +
      theme(plot.title = element_text(hjust = 0.5)) +
      ggtitle("Test Set Log Return Histogram & Model Densities"))
filename <- "test_set_log_returns_histogram_and_model_densities.png"
ggsave(filename, plot=p, width=10, height=8)

p <- (ggplot(test, aes(x=log_return)) +
      geom_function(aes(colour="Gaussian", linetype="Gaussian"),
                    fun=dnorm, n=1000, args=list(mean=train_mean, sd=sqrt(train_var), log=TRUE)) +
      geom_function(aes(colour=mixture_names[1], linetype=mixture_names[1]),
                    fun=mixture_log_density_vectorized, n=1000, args=list(mixture=mixture_models[[1]])) +
      geom_function(aes(colour=mixture_names[3], linetype=mixture_names[3]),
                    fun=mixture_log_density_vectorized, n=1000, args=list(mixture=mixture_models[[3]])) +
      geom_function(aes(color="Student's t", linetype="Student's t"),
                    fun=students_t_log_density, n=1000, args=list(params=students_t_params)) +
      scale_color_manual("Model", values=color_values) +
      scale_linetype_manual("Model", values=linetype_values) +
      xlim(c(-0.2, 0.2)) +
      theme_bw() +
      xlab("log return") +
      ylab("log of probability density") +
      theme(plot.title = element_text(hjust = 0.5)) +
      ggtitle("Model Log Densities"))
filename <- "model_log_densities.png"
ggsave(filename, plot=p, width=10, height=8)

## Inspect hmm_scaled_log_likelihood.png (saved above) for the 'corner' where the curve levels off
## and set hmm_chosen_order accordingly -- 3 matches the corner found for GE stock in the book; may
## differ for the S&P 500. Then show the MAP smoothing regime estimate for the chosen order.
hmm_chosen_order <- 3

chosen_fit <- relabel_states_by_variance(hmm_fits[[hmm_chosen_order]])  # Order states by increasing volatility
chosen_baum_welch <- baum_welch_gaussian_time_homogeneous(list(y=train_log_returns), chosen_fit)
regime <- get_map_smoothing_path(chosen_baum_welch$pi)

message("Fitted ", hmm_chosen_order, "-regime Gaussian HMM parameters (regimes ordered by increasing variance):")
print(rbind(m=chosen_fit$m, v=chosen_fit$v, mu=chosen_fit$mu))
print(chosen_fit$P)

hmm_df <- data.frame(date=train_dates_hmm, log_return=train_log_returns, close=train_close_hmm, regime=regime)

## Contiguous date ranges for each regime run, for background shading (lowest-volatility regime 1
## is left unshaded, as in Fig. 6.2)
regime_runs <- rle(hmm_df$regime)
run_ends <- cumsum(regime_runs$lengths)
run_starts <- c(1, head(run_ends, -1) + 1)
shading_df <- data.frame(regime=regime_runs$values,
                         start_date=hmm_df$date[run_starts],
                         end_date=hmm_df$date[run_ends])
shading_df <- subset(shading_df, regime > 1)

alpha_values <- seq(0.15, 0.6, length.out=hmm_chosen_order - 1)
names(alpha_values) <- seq(2, hmm_chosen_order)

p <- (ggplot(hmm_df, aes(x=date, y=log_return)) +
      geom_rect(data=shading_df, aes(xmin=start_date, xmax=end_date, ymin=-Inf, ymax=Inf, alpha=factor(regime)),
                fill="black", inherit.aes=FALSE) +
      geom_line() +
      scale_alpha_manual("Regime", values=alpha_values) +
      theme_bw() +
      xlab("") +
      ylab("log return") +
      theme(plot.title = element_text(hjust = 0.5)) +
      ggtitle(sprintf("S&P 500 Log Returns, %s-Regime MAP Smoothing Estimate", hmm_chosen_order)))
filename <- "hmm_regime_shading_log_return.png"
ggsave(filename, plot=p, width=10, height=6)

p <- (ggplot(hmm_df, aes(x=date, y=close)) +
      geom_rect(data=shading_df, aes(xmin=start_date, xmax=end_date, ymin=-Inf, ymax=Inf, alpha=factor(regime)),
                fill="black", inherit.aes=FALSE) +
      geom_line() +
      scale_alpha_manual("Regime", values=alpha_values) +
      theme_bw() +
      xlab("") +
      ylab("close") +
      theme(plot.title = element_text(hjust = 0.5)) +
      ggtitle(sprintf("S&P 500 Close, %s-Regime MAP Smoothing Estimate", hmm_chosen_order)))
filename <- "hmm_regime_shading_price.png"
ggsave(filename, plot=p, width=10, height=6)

