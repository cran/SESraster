## ---- include = FALSE---------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## ---- rand-spp, fig.height=4, fig.width=4-------------------------------------
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
                    }, t = t)
                  }, t = sample(seq(mn, mx), 2))
                }, r = r, mn = minmax(r)[1]+10, mx = minmax(r)[2]-10))

names(r) <- paste("sp", 1:nlyr(r))
plot(r)

## ---- ses-fun-----------------------------------------------------------------
appmean <- function(x, ...){
                      terra::app(x, "mean", ...)
}

## ---- ses-spat-sp, fig.height=4, fig.width=4----------------------------------
ses.sp <- SESraster(r, FUN = appmean, 
                    spat_alg = "bootspat_naive", spat_alg_args = list(random = "species"),
                    aleats = 5)
plot(ses.sp)

## ---- ses-spat-st, fig.height=4, fig.width=4----------------------------------
ses.st <- SESraster(r, FUN = appmean,
                    spat_alg = "bootspat_naive", spat_alg_args = list(random = "site"),
                    aleats = 5)
plot(ses.st)

## ---- ses-spat-na1, fig.height=4, fig.width=4---------------------------------
## let's create some missing values for layer/species 1
r2 <- r
set.seed(10)
cellsNA <- terra::spatSample(r2, 30, na.rm = TRUE, cells = TRUE, values = FALSE)
r2[cellsNA][1] <- NA
# plot(r)
set.seed(10)
sesNA <- SESraster(r2, FUN = appmean, FUN_args = list(na.rm = FALSE),
                   spat_alg = "bootspat_naive", spat_alg_args=list(random = "species"),
                   aleats = 5)
head(sesNA[cellsNA])
plot(sesNA)

## ---- ses-spat-na2, fig.height=4, fig.width=4---------------------------------
set.seed(10)
ses.woNA <- SESraster(r2, FUN = appmean, FUN_args = list(na.rm = TRUE), 
                      spat_alg = "bootspat_naive", spat_alg_args=list(random = "species"),
                      aleats = 5)
head(ses.woNA[cellsNA])
plot(ses.woNA)

## ---- ses-ftrait, fig.height=4, fig.width=4-----------------------------------
## example with `Fa_alg`
appsv <- function(x, lyrv, na.rm = FALSE, ...){
                      sumw <- function(x, lyrv, na.rm, ...){
                        ifelse(all(is.na(x)), NA,
                               sum(x*lyrv, na.rm=na.rm, ...))
                      }
                      stats::setNames(terra::app(x, sumw, lyrv = lyrv, na.rm=na.rm, ...), "sumw")
}

set.seed(10)
trait  <- sample(100:2000, nlyr(r))
trait

## ---- ses-trait1, fig.height=4, fig.width=4-----------------------------------
set.seed(10)
ses <- SESraster(r, FUN = appsv,
                 FUN_args = list(lyrv = trait, na.rm = TRUE),
                    Fa_sample = "lyrv",
                    Fa_alg = "sample", Fa_alg_args = list(replace = FALSE),
                    aleats = 5)
plot(ses)

## ---- ses-trait2, fig.height=4, fig.width=4-----------------------------------
set.seed(10)
ses <- SESraster(r, FUN = appsv,
                 FUN_args = list(lyrv = trait, na.rm = TRUE),
                    Fa_sample = "lyrv",
                    Fa_alg = "sample", Fa_alg_args = list(replace = TRUE),
                    aleats = 5)
plot(ses)

