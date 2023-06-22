## ---- include = FALSE---------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## ---- eval = FALSE------------------------------------------------------------
#  install.packages("SESraster")
#  library(SESraster)

## ---- eval = FALSE------------------------------------------------------------
#  require(devtools)
#  devtools::install_github("HemingNM/SESraster", build_vignettes = TRUE)
#  library(SESraster)

## ---- rand-spp, fig.height=8, fig.width=6-------------------------------------
library(SESraster)
library(terra)
# creating random species distributions
f <- system.file("ex/elev.tif", package="terra")
r <- rast(f)
set.seed(510)
r10 <- rast(lapply(1:18,
                function(i, r, mn, mx){
                  app(r, function(x, t){
                    sapply(x, function(x, t){
                       x<max(t) & x>min(t)
                    }, t=t)
                  }, t=sample(seq(mn, mx), 2))
                }, r=r, mn=minmax(r)[1]+10, mx=minmax(r)[2]-10))

names(r10) <- paste("sp", 1:nlyr(r10))
plot(r10)

## ---- naive-both, fig.height=8, fig.width=6-----------------------------------
srb <- bootspat_naive(r10, random = "both")
plot(srb, legend=F)

## ---- naive-both-rich, fig.height=4, fig.width=6------------------------------
plot(c(sum(r10), sum(srb)), main=c("observed", "randomized"))

## ---- naive-spp, fig.height=8, fig.width=6------------------------------------
sr1 <- bootspat_naive(r10, random = "species")
plot(sr1, legend=F)

## ---- naive-spp-rich, fig.height=4, fig.width=6-------------------------------
plot(c(sum(r10), sum(sr1)), main=c("observed", "randomized"))

## ---- naive-sppb, fig.height=8, fig.width=6-----------------------------------
sr1b <- bootspat_naive(r10, random = "species", memory = FALSE)

## ---- table-freq-naive--------------------------------------------------------
cbind(observed=sapply(r10, function(x)freq(x)[2,3]),
      randomized=sapply(sr1, function(x)freq(x)[2,3]),
      randomized_freq=sapply(sr1b, function(x)freq(x)[2,3]))

## ---- naive-site, fig.height=8, fig.width=6-----------------------------------
sr2 <- bootspat_naive(r10, random = "site")
plot(sr2, legend=F)

## ---- naive-site-rich, fig.height=4, fig.width=6------------------------------
plot(c(sum(r10), sum(sr2)), main=c("observed", "randomized"))

## -----------------------------------------------------------------------------
# bootstrapping once
fr.prob <- SESraster::fr2prob(r10)
prob <- terra::app(r10,
                   function(x){
                     ifelse(is.na(x), 0, 1)
                   })

randr10 <- bootspat_str(r10, rprob = prob, fr_prob = fr.prob)

## ---- str-site, fig.height=8, fig.width=6-------------------------------------
plot(randr10, legend=F)

## ---- str-site-rich, fig.height=4, fig.width=6--------------------------------
plot(c(sum(r10), sum(randr10)), main=c("observed", "randomized"))

## ---- table-freq--------------------------------------------------------------
cbind(observed=sapply(r10, function(x)freq(x)[2,3]),
      randomized=sapply(randr10, function(x)freq(x)[2,3]))

