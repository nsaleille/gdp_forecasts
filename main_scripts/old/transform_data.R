###################################################
################	Data Transformations	#######
###################################################

# Prepare data for predictive analysis
# 1. harmanize frequency: interpolation of quarterly series to monthly series using cubic splines
# 2. harmonize dates: bind series and omit NAs to get a dataset with common start / end values
# 3. transform ta stationarity: test for stationnarity and differentiate integrated series.
# 4. standardize variable moments: substract mean and divide by one standard deviation.

rm(list=ls(all=TRUE))
library(R.utils)
setwd("/Users/nsaleille/Dropbox/ofpr/scripts") # chemin perso
load('./full_set_of_series.RData')
load('./full_description_of_series.RData')
sourceDirectory('./fonctions/zoo_utils', modifiedOnly = FALSE)
sourceDirectory('./fonctions/extract_data', modifiedOnly = FALSE)

# for series with mixed frequency
# suppress observations above maxgap of 3 periods
# interpolate on a monthly basis

nas <- which(sapply(data.full, function(x) return(sum(is.na(x$zoo)))) > 5)
search.full$desc[nas]
plot(data.full[nas][[1]]$zoo, type = 'p', col = 'red')
filtered.series <- lapply(get_zoo(data.full[nas]), cubicSplineInterp, by = 'month', maxgap = 3)
data.full[nas] <- replace_zoo(data.full[nas], filtered.series)
lines(data.full[nas][[1]]$zoo)

## select series with sufficient number of observations

keep <- !((search.full$info['start'] > 1991) | (search.full$info['end'] < 2014))
data.full <- data.full[which(keep)]
# print dropped series
search.full$info[which(!keep),]
search.full$desc[which(!keep)]
search.keep <- list(info = search.full$info[which(keep),], desc = search.full$desc[which(keep)])

# set up data for pseudo real-time forecasting
# i.e. lag series with release periods > 0

# données enquêtes insee release = M+0
# données réelles insee release = M+1
sources <- sapply(data.full, function(x) x$source)
search.full$desc[which(sources == 'insee')]
enquetes <- search_print(c('enquête|comptes'), search.full$desc[which(sources == 'insee')], search.full$info[which(sources == 'insee'),])
reel <- which(!(rownames(search.full$info[which(sources == 'insee'),]) %in% rownames(enquetes$info)))
data.full[reel] <- lapply(data.full[reel], function(x) {x$release <- 1; return(x)})

# apply lags depending on release
release <- sapply(data.full, function(x) x$release)
for (i in unique(release)){
 data.full[which(release == i)] <- lapply(data.full[which(release == i)], function(x) {lag(x$zoo, -i); return(x)})
}

# mensualisation des séries
library(zoo)
data.mens <- data.full[sapply(data.full, FUN = function(x) (x$freq == 'mois'))]
data.trim <- data.full[sapply(data.full, FUN = function(x) (x$freq == 'Trimestre'))]
series.trim <- lapply(get_zoo(data.trim), cubicSplineInterp, by = 'month', maxgap = 3)
data.trim.interp <- replace_zoo(data.trim, series.trim)

# données sphéricisées
x <- unlist(list(data.mens, data.trim.interp), recursive = FALSE)
xsp <- Reduce(function(x,y) merge(x,y), get_zoo(x))
colnames(xsp) <- sapply(x, function(x) x$id)
xsp <- na.omit(xsp)

dPIB <- na.omit(get_series('X001688995')$zoo)
y <- appendZooIndex(dPIB)
y <- (y - mean(dPIB)) / sd(dPIB)
y.trim <- window(dPIB, start = as.yearqtr(start(xsp)), end = end(dPIB))
y.trim <- (y.trim - mean(dPIB)) / sd(dPIB)
y <- merge(xsp, y)$y

plot(y.trim)
lines(y, type = 'p', col = 'red')

# stationnarisation puis centrage/réduction (ordre donné par SW10)
# on stationnarise avant de centrer / réduire (logique)

# test for stationarity
library(tseries)
tests <- lapply(xsp, adf.test, alternative = 'stationary')
tests.bool <- sapply(tests, function(x) (x$p.value > 0.5))
sum(tests.bool)
dxsp <- merge(xsp[ , tests.bool == FALSE], diff(xsp[ , tests.bool == TRUE]))
dxsp <- na.omit(dxsp)
tests <- lapply(dxsp, adf.test, alternative = 'stationary')
tests.bool <- sapply(tests, function(x) (x$p.value > 0.5))
sum(tests.bool)

# etrange on atteint pas la stationnarité en différenciant
# il faut peut être traiter ces séries autrement...

# variables centree - réduites
predictors <- apply2zooCols(dxsp, FUN = function(x) x - mean(x))
predictors <- apply2zooCols(predictors, FUN = function(x) x / sd(x))
outcome <- y

save(predictors, file = './predictors.RData')
save(outcome, file = './outcome.RData')
