## Gaussian-emission variant of the discrete-hidden-state, time-homogeneous HMM functions in
## hmm_functions.R (baum_welch_time_homogeneous / em_parameter_estimates_time_homogeneous /
## viterbi_path_time_homogeneous). Same forward-backward / EM / Viterbi skeleton, following Ramon
## van Handel's HMM notes (https://www.princeton.edu/~rvan/orf557/hmm080728.pdf, algorithms 3.2,
## 3.4, 6.1), but observations are continuous instead of discrete symbols:
##
##   Y_k = m(X_k) + sqrt(v(X_k)) * eta_k,    eta_k iid N(0, 1)
##
## as in sec. 6.3's General Electric stock regime-switching example (Fig. 6.2), where X_k is the
## discrete hidden regime, m is the per-regime mean log return, and v is the per-regime variance.

valid_parameters_gaussian_time_homogeneous <- function(params) {
    stopifnot(is.list(params))
    stopifnot("n_components" %in% names(params))
    stopifnot("mu" %in% names(params))  # Vector of probabilities for initial distribution
    stopifnot(length(params$mu) == params$n_components)
    stopifnot(all(params$mu >= 0))
    stopifnot(isTRUE(all.equal(sum(params$mu), 1.0)))
    stopifnot("P" %in% names(params))  # Time-invariant transition matrix
    stopifnot(isTRUE(all.equal(rowSums(params$P), rep(1, nrow(params$P)))))
    stopifnot("m" %in% names(params))  # Mean of Y conditional on hidden state
    stopifnot("v" %in% names(params))  # Variance of Y conditional on hidden state
    stopifnot(length(params$m) == params$n_components)
    stopifnot(length(params$v) == params$n_components)
    stopifnot(all(params$v > 0))
    return(TRUE)
}

valid_panel_element_gaussian <- function(panel_element) {
    stopifnot(is.list(panel_element))
    stopifnot("y" %in% names(panel_element))
    stopifnot(is.numeric(panel_element$y))
    return(TRUE)
}

get_random_initial_parameters_gaussian <- function(n_components, y, diag_min=0.9, diag_max=0.999) {
    ## Random initial parameters for Gaussian HMM EM. Following the recipe in van Handel's notes
    ## sec. 6.3 (GE stock example): initialize P close to the identity, since we don't expect the
    ## regime to switch on a daily basis, and spread the initial (m, v) around the empirical
    ## quantiles/variance of y. The initial-state distribution mu is set to P's stationary
    ## distribution (see the mu_update argument of get_em_estimates_gaussian_time_homogeneous).
    stopifnot(0 < diag_min && diag_min < diag_max && diag_max < 1)
    y_no_na <- y[!is.na(y)]

    if(n_components == 1) {
        P <- matrix(1, 1, 1)
    } else {
        random_uniform <- runif(n_components, min=diag_min, max=diag_max)
        P <- matrix((1 - random_uniform) / (n_components - 1), n_components, n_components)
        diag(P) <- random_uniform
    }

    mu <- get_stationary_distribution(P)

    m <- as.numeric(quantile(y_no_na, probs=seq(0, 1, length.out=n_components + 2))[seq(2, n_components + 1)])
    m <- m + rnorm(n_components, sd=0.1 * sd(y_no_na))
    v <- var(y_no_na) * runif(n_components, min=0.5, max=1.5)

    return(list(n_components=n_components, mu=mu, P=P, m=m, v=v))
}

baum_welch_gaussian_time_homogeneous <- function(panel_element, params) {
    ## Scaled forward-backward algorithm, Gaussian observation densities in place of the discrete
    ## Pr[Y | S] lookup in baum_welch_time_homogeneous().
    stopifnot(valid_panel_element_gaussian(panel_element))
    stopifnot(valid_parameters_gaussian_time_homogeneous(params))
    y_length <- length(panel_element$y)
    stopifnot(y_length > 1)

    get_upsilon <- function(y_k) {
        if(is.na(y_k)) {
            return(rep(1, params$n_components))
        } else {
            return(dnorm(y_k, mean=params$m, sd=sqrt(params$v)))
        }
    }

    c <- vector("numeric", y_length)
    upsilon <- get_upsilon(panel_element$y[1])
    mu <- params$mu
    c[1] <- sum(upsilon * mu)
    ## Matrix pi_contemporaneous gives probabilities over x_k conditional on {y_1, ... , y_k}
    pi_contemporaneous <- matrix(NA, params$n_components, y_length)
    pi_contemporaneous[, 1] <- upsilon * mu / c[1]
    upsilon_list <- list()
    upsilon_list[[1]] <- upsilon
    for(k in seq(2, y_length)) {
        ## Forward loop
        upsilon <- get_upsilon(panel_element$y[k])
        upsilon_list[[k]] <- upsilon  # Cache for backward loop
        pi_tilde <- upsilon * t(params$P) %*% pi_contemporaneous[, k - 1]
        c[k] <- sum(pi_tilde)
        pi_contemporaneous[, k] <- pi_tilde / c[k]
    }

    beta <- matrix(NA, params$n_components, y_length)
    beta[, y_length] <- 1 / c[y_length]
    ## Matrix pi gives probabilities over x conditional on the full history of y (smoothing)
    pi <- matrix(NA, params$n_components, y_length)
    pi[, y_length] <- pi_contemporaneous[, y_length]
    pi_transition_list <- list()
    for(k in seq(1, y_length - 1)) {
        ## Backward loop
        upsilon <- diag(upsilon_list[[y_length - k + 1]], params$n_components, params$n_components)
        pi_matrix <- diag(pi_contemporaneous[, y_length - k], params$n_components, params$n_components)
        beta_matrix <- diag(beta[, y_length - k + 1], params$n_components, params$n_components)
        beta[, y_length - k] <- params$P %*% upsilon %*% beta[, y_length - k + 1] / c[y_length - k]
        pi_transition_list[[y_length - k]] <- pi_matrix %*% params$P %*% upsilon %*% beta_matrix
        stopifnot(isTRUE(all.equal(sum(pi_transition_list[[y_length - k]]), 1.0)))
        pi[, y_length - k] <- rowSums(pi_transition_list[[y_length - k]])
    }

    loglik <- sum(log(c))
    ## c[k] = Pr(y_k | y_1, ..., y_{k-1}) (Pr(y_1) for k=1), the one-step-ahead predictive density
    ## implied by params at each point -- returned so callers can score a held-out subsequence's
    ## predictive log-likelihood without having to re-derive it from pi/pi_transition_list.
    return(list(loglik=loglik, pi=pi, pi_transition_list=pi_transition_list, c=c))
}

get_em_estimates_gaussian_time_homogeneous <- function(panel, params, max_iter, epsilon=0.001,
                                                       mu_update=c("stationary", "em")) {
    ## EM for a panel of independent Gaussian-emission HMM realizations (a single log-return
    ## series is the panel=list(list(y=...)) case). Stops at max_iter or once the sup-norm
    ## distance between successive parameter estimates drops below epsilon.
    ##
    ## mu_update controls the initial-state distribution mu:
    ##   "stationary" (default): mu is tied to the stationary distribution of the current P, i.e. we
    ##     assume the regime chain has been running long before the first observation. For a single
    ##     series, the EM M-step for mu is the smoothed posterior at k=1 -- it rests on one
    ##     observation, collapses to a point mass, and swings between iterations along the way,
    ##     which can dominate the sup-norm stopping distance.
    ##   "em": the standard Baum-Welch M-step (average of the k=1 smoothed posteriors across panel
    ##     elements). Sensible for a panel of many independent sequences.
    ## Note that "stationary" is not an exact M-step for mu (the stationary constraint couples mu to
    ## P), so the log-likelihood is not guaranteed to increase monotonically. mu only enters the
    ## likelihood through the first observation, so in practice the effect is negligible.
    mu_update <- match.arg(mu_update)
    message("starting gaussian em, time is ", Sys.time(), ", initial (m, v) are:")
    print(rbind(m=params$m, v=params$v))

    if(mu_update == "stationary") params$mu <- get_stationary_distribution(params$P)
    stopifnot(valid_parameters_gaussian_time_homogeneous(params))
    observation_lengths <- vapply(panel, function(panel_element) length(panel_element$y), FUN.VALUE=1)
    stopifnot(all(observation_lengths > 1) && length(unique(observation_lengths)) == 1)

    n_em_iterations <- 0  # Number of EM updates applied, == length(params$loglik) on return
    converged <- FALSE
    for(iteration in seq_len(max_iter)) {
        baum_welch_list <- lapply(panel, baum_welch_gaussian_time_homogeneous, params=params)
        panel_with_baum_welch <- Map(c, panel, baum_welch_list)

        ## Update transition matrix
        numerators <- unlist(lapply(baum_welch_list, function(x) x$pi_transition_list), recursive=FALSE)
        denominators <- unlist(lapply(baum_welch_list, function(x) {
            lapply(seq_len(ncol(x$pi) - 1), function(time) {
                matrix(x$pi[, time], params$n_components, params$n_components)
            })
        }), recursive=FALSE)
        numerator <- Reduce("+", numerators)
        denominator <- Reduce("+", denominators)
        stopifnot(all(denominator > 0))  # Can fail if a state has zero probability
        updated_P <- numerator / denominator
        stopifnot(isTRUE(all.equal(rowSums(updated_P), rep(1, params$n_components))))

        ## Update initial distribution over hidden state
        if(mu_update == "stationary") {
            updated_mu <- get_stationary_distribution(updated_P)
        } else {
            mus <- lapply(baum_welch_list, function(x) x$pi[, 1])
            updated_mu <- Reduce("+", mus) / length(panel)
        }
        stopifnot(isTRUE(all.equal(sum(updated_mu), 1)))

        ## Update per-state mean and variance (weighted MLE; careful, y can contain NAs)
        state_weights <- lapply(panel_with_baum_welch, function(panel_element) {
            y_non_na <- matrix(1 * !is.na(panel_element$y), params$n_components, length(panel_element$y), byrow=TRUE)
            rowSums(panel_element$pi * y_non_na)
        })
        total_state_weight <- Reduce("+", state_weights)

        weighted_y_sums <- lapply(panel_with_baum_welch, function(panel_element) {
            y_filled <- ifelse(is.na(panel_element$y), 0, panel_element$y)
            y_matrix <- matrix(y_filled, params$n_components, length(panel_element$y), byrow=TRUE)
            rowSums(panel_element$pi * y_matrix)
        })
        updated_m <- Reduce("+", weighted_y_sums) / total_state_weight

        weighted_sq_dev_sums <- lapply(panel_with_baum_welch, function(panel_element) {
            sapply(seq_len(params$n_components), function(state) {
                deviations_sq <- (panel_element$y - updated_m[state])^2
                deviations_sq[is.na(deviations_sq)] <- 0
                sum(panel_element$pi[state, ] * deviations_sq)
            })
        })
        updated_v <- Reduce("+", weighted_sq_dev_sums) / total_state_weight

        P_distance <- max(abs(params$P - updated_P))
        mu_distance <- max(abs(params$mu - updated_mu))
        m_distance <- max(abs(params$m - updated_m))
        v_distance <- max(abs(params$v - updated_v))
        distance <- max(P_distance, mu_distance, m_distance, v_distance)

        loglik <- sum(vapply(baum_welch_list, function(x) x$loglik, FUN.VALUE=1))
        message("iteration ", iteration, " distance ", round(distance, 6), " loglik ", round(loglik, 4))

        params$P <- updated_P
        params$mu <- updated_mu
        params$m <- updated_m
        params$v <- updated_v
        params$distance <- c(params$distance, distance)  # c(NULL, x) == x, so this works on iteration 1 too
        params$loglik <- c(params$loglik, loglik)
        params$panel_size <- length(panel)
        n_em_iterations <- iteration

        if(distance < epsilon) {
            converged <- TRUE
            break
        }
        rm(panel_with_baum_welch)
        rm(baum_welch_list)
        gc()
    }
    message("done running gaussian em, time is ", Sys.time())
    params$n_em_iterations <- n_em_iterations
    params$converged <- converged
    params$time_finished_em <- Sys.time()
    return(params)
}

viterbi_path_gaussian_time_homogeneous <- function(panel_element, params) {
    ## Viterbi algorithm (most likely single joint path for x), Gaussian densities in place of the
    ## discrete Pr[Y | S] lookup in viterbi_path_time_homogeneous().
    stopifnot(valid_panel_element_gaussian(panel_element))
    stopifnot(valid_parameters_gaussian_time_homogeneous(params))

    get_upsilon <- function(y_k) {
        if(is.na(y_k)) {
            return(rep(1, params$n_components))
        } else {
            return(dnorm(y_k, mean=params$m, sd=sqrt(params$v)))
        }
    }

    mu <- params$mu
    t_max <- length(panel_element$y)
    stopifnot(t_max >= 2)
    v <- matrix(NA, t_max, params$n_components)  # Maximized log likelihoods
    v[1, ] <- log(mu) + log(get_upsilon(panel_element$y[1]))
    b <- matrix(NA, t_max, params$n_components)
    for(k in seq(2, t_max)) {
        upsilon <- get_upsilon(panel_element$y[k])
        for(i in seq_len(params$n_components)) {
            b[k, i] <- which.max(v[k - 1, ] + log(params$P[, i]))
            v[k, i] <- v[k - 1, b[k, i]] + log(params$P[b[k, i], i]) + log(upsilon[i])
        }
    }
    stopifnot(all(!is.na(v)))
    most_likely_path <- rep(NA, t_max)
    most_likely_path[t_max] <- which.max(v[t_max, ])
    for(k in seq(1, t_max - 1)) {
        most_likely_path[t_max - k] <- b[t_max - k + 1, most_likely_path[t_max - k + 1]]
    }
    stopifnot(all(!is.na(most_likely_path)))
    return(most_likely_path)
}

get_map_smoothing_path <- function(pi_matrix) {
    ## MAP smoothing estimate of the regime at each time k: argmax_i Pr[X_k = i | y_1, ... , y_n]
    ## (as opposed to viterbi_path_gaussian_time_homogeneous(), which returns the single most
    ## likely joint path). This is the shading convention used in Fig. 6.2.
    return(apply(pi_matrix, 2, which.max))
}

fit_gaussian_hmm <- function(y, n_components, n_random_starts=5, max_iter=50, epsilon=0.001,
                             diag_min=0.9, diag_max=0.999, mu_update="stationary") {
    ## Fit a time-homogeneous Gaussian-emission HMM to a single log-return series y by EM,
    ## with n_random_starts random initializations (see get_random_initial_parameters_gaussian);
    ## returns the fit with the highest final log-likelihood.
    panel <- list(list(y=y))
    initial_parameters <- replicate(n_random_starts,
                                    get_random_initial_parameters_gaussian(n_components, y, diag_min=diag_min, diag_max=diag_max),
                                    simplify=FALSE)
    fits <- lapply(initial_parameters, function(params) {
        get_em_estimates_gaussian_time_homogeneous(panel, params, max_iter=max_iter, epsilon=epsilon,
                                                   mu_update=mu_update)
    })
    final_logliks <- sapply(fits, function(x) tail(x$loglik, 1))
    best_fit <- fits[[which.max(final_logliks)]]
    best_fit$all_final_logliks <- final_logliks
    return(best_fit)
}

get_stationary_distribution <- function(P) {
    ## Stationary distribution of a time-homogeneous Markov transition matrix P (rows sum to 1):
    ## the pi such that pi %*% P == pi. Used as the initial-state distribution mu (see the
    ## mu_update argument of get_em_estimates_gaussian_time_homogeneous).
    stopifnot(isTRUE(all.equal(rowSums(P), rep(1, nrow(P)))))
    n <- nrow(P)
    if(n == 1) return(1)
    eigen_decomp <- eigen(t(P))
    stationary_index <- which.min(abs(eigen_decomp$values - 1))
    stationary <- Re(eigen_decomp$vectors[, stationary_index])
    stationary <- stationary / sum(stationary)  # Eigenvector sign/scale is arbitrary; fix both
    stationary <- pmax(stationary, 0)  # Clip tiny negative numerical noise
    stationary <- stationary / sum(stationary)
    return(stationary)
}

relabel_states_by_variance <- function(params) {
    ## EM recovers states up to an arbitrary relabeling. Canonically order states by increasing
    ## variance, matching the low/medium/high volatility regime convention in Fig. 6.2.
    order_idx <- order(params$v)
    params$mu <- params$mu[order_idx]
    params$m <- params$m[order_idx]
    params$v <- params$v[order_idx]
    params$P <- params$P[order_idx, order_idx]
    return(params)
}
