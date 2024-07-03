library(tidyverse)
library(metafor)
devtools::load_all()

if(!dir.exists("slides/00-multiverse-meta-analysis/data/plessen2023")){
  # download from osf
}

dat <- readr::read_csv("slides/00-multiverse-meta-analysis/data/plessen2023/data/tidy/data_cleaned.csv")

names(dat)[1] <- "author"
dat <- escalc("GEN", yi = yi, vi = vi, data = dat)
dat <- mutate(dat, across(where(is.character), factor))
dat <- aggregate(dat, cluster = author, rho = 0)

specif <- dat |>
  select(target_group, format, diagnosis, risk_of_bias, type) |> 
  distinct() |> 
  tibble()

mfilter <- function(data, conds, to0 = FALSE){
  res <- vector(mode = "list", length = nrow(conds))
  coln <- colnames(conds)
  for(i in 1:length(res)){
    colv <- as.character(unlist(conds[i, ]))
    ss <- sprintf("%s == '%s'", coln, colv)
    ex <- paste(ss, collapse = " & ")
    res[[i]] <- filter(data, !!rlang::parse_expr(ex))
  }
  if(length(res) == 1){
    unlist(res, recursive = FALSE)
  }else{
    res
  }
}

specif$data <- mfilter(dat, specif)

rma.ee <- function(x){
  rma(yi, vi, data = x, method = "EE")
}

rma.re <- function(x){
  rma(yi, vi, data = x, method = "REML")
}

specif$rma.ee <- map(specif$data, rma.ee)
specif$rma.re <- map(specif$data, rma.re)

specif <- pivot_longer(specif, 
                       c(rma.ee, rma.re),
                       names_to = "model",
                       values_to = "fit")

specif$method <- sapply(specif$fit, function(x) x$method)

tau2 <- rma(yi, vi, method = "REML", data = dat)$tau2

B <- 1000
S <- matrix(NA, nrow = nrow(specif), ncol = B)

for(i in 1:nrow(specif)){
  data <- specif$data[[i]]
  vi <- data$vi
  v <- vi + tau2
  k <- nrow(data)
  method <- specif$method[i]
  for(j in 1:B){
    yi <- rnorm(k, 0, sqrt(v))
    if(method == "REML"){
      b <- tryCatch({
        rma_optim(yi, vi)$par[[1]]
      },
      error = function(e){
        rma(yi, vi, method = "DL")$b[[1]]
      })
    }else{
      b <- weighted.mean(yi, 1/vi)
    }
    S[i, j] <- b
  }
}

S_sort <- apply(S, 2, sort)

SD <- data.frame(id = rep(1:nrow(S_sort), B), b = c(S_sort),
                 spec = rep(1:B, each = nrow(S_sort)))

specif <- list(
  specif = specif,
  all = dat,
  SD = SD
)

saveRDS(specif, "slides/00-multiverse-meta-analysis/objects/plessen2023-specification-data.rds")
