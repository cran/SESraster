## ----include = FALSE----------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## ----eval = FALSE-------------------------------------------------------------
#  install.packages("SESraster")

## ----eval = FALSE-------------------------------------------------------------
#  require(devtools)
#  devtools::install_github("HemingNM/SESraster", build_vignettes = TRUE)

## ----rand-spp, fig.height=8, fig.width=6--------------------------------------
library(SESraster)
library(terra)
# creating random species distributions
f <- system.file("ex/elev.tif", package="terra")
r <- rast(f)
set.seed(510)
r <- rast(lapply(1:18,
                function(i, r, mn, mx){
                  app(r, function(x, t){
                    sapply(x, function(x, t){
                       x<max(t) & x>min(t)
                    }, t=t)
                  }, t=sample(seq(mn, mx), 2))
                }, r=r, mn=minmax(r)[1]+10, mx=minmax(r)[2]-10))

names(r) <- paste("sp", 1:nlyr(r))
plot(r)

## ----naive-both, fig.height=8, fig.width=6------------------------------------
srb <- bootspat_naive(r, random = "both")
plot(srb, legend=F)

## ----naive-both-rich, fig.height=4, fig.width=6-------------------------------
plot(c(sum(r), sum(srb)), main=c("observed", "randomized"))

## ----naive-spp, fig.height=8, fig.width=6-------------------------------------
sr1 <- bootspat_naive(r, random = "species")
plot(sr1, legend=F)

## ----naive-spp-rich, fig.height=4, fig.width=6--------------------------------
plot(c(sum(r), sum(sr1)), main=c("observed", "randomized"))

## ----naive-sppb, fig.height=8, fig.width=6------------------------------------
sr1b <- bootspat_naive(r, random = "species", memory = FALSE)

## ----table-freq-naive---------------------------------------------------------
cbind(observed=sapply(r, function(x)freq(x)[2,3]),
      randomized=sapply(sr1, function(x)freq(x)[2,3]),
      randomized_freq=sapply(sr1b, function(x)freq(x)[2,3]))

## ----naive-site, fig.height=8, fig.width=6------------------------------------
sr2 <- bootspat_naive(r, random = "site")
plot(sr2, legend=F)

## ----naive-site-rich, fig.height=4, fig.width=6-------------------------------
plot(c(sum(r), sum(sr2)), main=c("observed", "randomized"))

## -----------------------------------------------------------------------------
# bootstrapping once
fr.prob <- SESraster::fr2prob(r)
prob <- terra::app(r,
                   function(x){
                     ifelse(is.na(x), 0, 1)
                   })

randr10 <- bootspat_str(r, rprob = prob, fr_prob = fr.prob)

## ----str-site, fig.height=8, fig.width=6--------------------------------------
plot(randr10, legend=F)

## ----str-site-rich, fig.height=4, fig.width=6---------------------------------
plot(c(sum(r), sum(randr10)), main=c("observed", "randomized"))

## ----table-freq---------------------------------------------------------------
cbind(observed=sapply(r, function(x)freq(x)[2,3]),
      randomized=sapply(randr10, function(x)freq(x)[2,3]))

## -----------------------------------------------------------------------------
# bootstrapping once
randff <- bootspat_ff(r)

## ----str-ff, fig.height=4, fig.width=6----------------------------------------
obs_fr <- unlist(terra::global(r, function(x) sum(x, na.rm = TRUE)))
v_seq <- order(obs_fr)
plot(c(r[[v_seq[length(v_seq)-1:4]]], randff[[v_seq[length(v_seq)-1:4]]]), 
     nr=2, main=paste(rep(c("original", "null"), each=4), names(r[[v_seq[length(v_seq)-1:4]]])))
plot(c(r[[v_seq[floor(length(v_seq)/2) + 2:-1]]], randff[[v_seq[floor(length(v_seq)/2)+ 2:-1]]]),
     nr=2, main=paste(rep(c("original", "null"), each=4), names(r[[v_seq[floor(length(v_seq)/2)+ 2:-1]]])))
plot(c(r[[v_seq[4:1]]], randff[[v_seq[4:1]]]), 
     nr=2, main=paste(rep(c("original", "null"), each=4), names(r[[v_seq[4:1]]])))


## ----str-ff-rich, fig.height=4, fig.width=6-----------------------------------
plot(c(sum(r), sum(randff), sum(r)-sum(randff)), main=c("observed", "randomized", "difference"), nr=1)

## ----table-freqff-------------------------------------------------------------
cbind(observed=sapply(r, function(x)freq(x)[2,3]),
      randomized=sapply(randff, function(x)freq(x)[2,3]))

