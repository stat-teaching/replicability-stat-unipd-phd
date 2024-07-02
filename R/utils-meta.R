sim_studies <- function(k, theta, tau2, n0, n1, summary = FALSE){
  yi <- rnorm(k, theta, sqrt(tau2 + 1/n0 + 1/n1))
  vi <- (rchisq(k, n0 + n1 - 2) / (n0 + n1 - 2)) * (1/n0 + 1/n1)
  out <- data.frame(yi, vi, sei = sqrt(vi))
  if(summary){
    out <- summary_es(out)
  }
  return(out)
}

summary_es <- function(data){
  out <- metafor::escalc(yi = yi, vi = vi, sei = sei, data = data)
  data.frame(summary(out))
}

quick_forest <- function(data, interval = TRUE, weigth = FALSE, size = 20){
  k <- nrow(data)
  data$id <- 1:k
  data$lower <- data$yi - 2*data$sei
  data$upper <- data$yi + 2*data$sei
  xlim <- c(min(data$lower) - 1, max(data$upper) + 1)

  ggplot(data) + {
  if(weigth){
    geom_point(aes(x = yi, y = id, size = 1/vi),
             shape = 15,
             show.legend = FALSE)
  }else{
    geom_point(aes(x = yi, y = id),
             shape = 15,
             size = 3)
  }
} + {
  if(interval){
    geom_segment(aes(x = lower, y = id, 
                   xend = upper, yend = id))
  }
} +
  scale_y_continuous(breaks = 1:k) +
  xlim(xlim) +
  xlab("Effect Size") +
  theme_minimal(size) +
  theme(axis.title.y = element_blank())
}


# parse_pb_criteria <- function(data, expression){
#   with(data, eval(parse(text = expression)))
# }

# sim_pub_bias <- function(ps, 
#                          pns,
#                          criteria,
#                          k, 
#                          theta, 
#                          tau2, 
#                          min_n, 
#                          max_n){
#   
#   criteria <- deparse(substitute(criteria))
#   res <- vector(mode = "list", length = k)
#   i <- 1
#   
#   while(i <= k){
#     n <- round(runif(1, min_n, max_n))
#     dat_i <- sim_studies(k = 1, theta = 0.5, tau2 = tau2, n0 = n, n1 = n)
#     dat_i$n <- n
#     dat_i <- escalc(yi = yi, vi = vi, sei = sei, data = dat_i)
#     dat_i <- data.frame(summary(dat_i))
#     pub_criteria <- parse_pb_criteria(dat_i, criteria)
#     
#     if(pub_criteria){
#       if(rbinom(1, 1, ps) == 1){
#         res[[i]] <- dat_i
#         i <- i + 1
#       }
#     }else{
#       if(rbinom(1, 1, pns) == 1){
#         res[[i]] <- dat_i
#         i <- i + 1
#       }
#     }
#   }
#   
#   dat <- do.call(rbind, res)
#   return(dat)
# }

sim_pub_bias <- function(selmodel,
                         k, 
                         theta, 
                         tau2, 
                         nmin, 
                         nmax){
  selmodel$method <- match.arg(selmodel$method, choices = c("custom", "2step", "beta"))
  res <- vector(mode = "list", length = k)
  i <- 1
  while(i <= k){
    n <- round(runif(1, nmin, nmax))
    dat_i <- sim_studies(k = 1, theta = theta, tau2 = tau2, n0 = n, n1 = n)
    dat_i$n <- n
    dat_i <- metafor::escalc(yi = yi, vi = vi, sei = sei, data = dat_i)
    dat_i <- data.frame(summary(dat_i))
    
    if(selmodel$method == "2step"){
      ppub <- weigth_2step(x = dat_i[[selmodel$param]], th = selmodel$th, side = selmodel$side)
    }else if(selmodel$method == "beta"){
      ppub <- weigth_beta(x = dat_i$pval, a = selmodel$a, b = selmodel$b)
    }else{
      ppub <- with(dat_i, eval(parse(text = selmodel$operation)))
    }
    
    if(rbinom(1, 1, ppub) == 1){
      res[[i]] <- dat_i
      i <- i + 1
    }
  }
  
  dat <- do.call(rbind, res)
  return(dat)
}

weigth_beta <- function(x, a, b){
  x^(a - 1) * (1 - x)^(b - 1)
}

weigth_2step <- function(x, th, side = "<="){
  ifelse(eval(call(side, x, th)), 1, 0)
}

meta_REML <- function(x, yi, vi){
  theta <- x[1]
  tau2 <- x[2]
  -0.5 * sum(log(tau2 + vi)) - 0.5 * log(sum(1/(tau2 + vi))) - 0.5 * sum((yi - theta)^2 / (tau2 + vi))
}

rma_optim <- function(yi, vi, prior = c(0, 0)){
  optim(par = prior,
        meta_REML,
        yi = yi,
        vi = vi,
        method = "L-BFGS-B",
        lower = c(-Inf, 0),
        upper = c(Inf, Inf),
        hessian = TRUE,
        control = list(fnscale=-1))
}

.get_rma_se <- function(vi, tau2 = 0){
  wi <- sum(1/(vi + tau2))
  sqrt(1 / wi)
}

.rma_fe <- function(yi, vi){
  theta <- weighted.mean(yi, 1/vi)
  se = .get_rma_se(vi)
  theta/se
}

.rma_reml <- function(yi, vi){
  res <- rma_optim(yi, vi)
  se = .get_rma_se(vi, res$par[2])
  res$par[1]/se
}