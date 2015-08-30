# France raw loading v1
# source: INSEE
# master thesis Nicolas Saleille

rm(list=ls(all=TRUE))
setwd("/Users/nsaleille/Dropbox/ofpr/scripts") # chemin perso
source('./fonctions/insee_xls_to_zoo_v3.R')
source('./fonctions/extractInseeInfo.R')

files.conjoncture <- list.files("./data/insee/enquetes_conjoncture")
paths.conjoncture <- paste('./data/insee/enquetes_conjoncture/', files.conjoncture, sep='')
files.autres <- list.files("./data/insee/autres")
paths.autres <- paste('./data/insee/autres/', files.autres, sep='')

library(xtable)
raw.conjoncture <- extractInseeInfo(paths.conjoncture)
print(xtable(data.frame(raw.conjoncture$desc)), file = '../tex/conjoncture_desc.tex', size = 'footnotesize')
print(xtable(raw.conjoncture$info), file = '../tex/conjoncture_info.tex')

raw.autres <- extractInseeInfo(paths.autres[1:4])
desc <- data.frame(raw.autres$desc)
rownames(desc) <- rownames(raw.autres$info)
print(xtable(desc), file = '../tex/autres_desc.tex', size = 'footnotesize')
print(xtable(raw.autres$info), file = '../tex/autres_info.tex')

raw.autres2 <- extractInseeInfo(paths.autres[5:9])
desc2 <- data.frame(raw.autres2$desc)
rownames(desc2) <- rownames(raw.autres2$info)
print(xtable(desc2), file = '../tex/autres_desc2.tex', size = 'footnotesize')
print(xtable(raw.autres2$info), file = '../tex/autres_info2.tex')

raw.autres3 <- extractInseeInfo(paths.autres[10:13])
desc3 <- data.frame(raw.autres3$desc)
rownames(desc3) <- rownames(raw.autres3$info)
print(xtable(desc3), file = '../tex/autres_desc3.tex', size = 'footnotesize')
print(xtable(raw.autres3$info), file = '../tex/autres_info3.tex')

raw.autres4 <- extractInseeInfo(paths.autres[14:length(paths.autres)])
desc4 <- data.frame(raw.autres4$desc)
rownames(desc4) <- rownames(raw.autres4$info)
print(xtable(desc4), file = '../tex/autres_desc4.tex', size = 'footnotesize')
print(xtable(raw.autres4$info), file = '../tex/autres_info4.tex')

data <- insee_xls_to_zoo(paths.conjoncture[2])
series <- sapply(data, function(x) x$zoo)
series <- sapply(series, function(x){coredata(x) <- as.numeric(coredata(x)); return(x)})
for (i in series){
  plot(i)
}

typeof(coredata(series[[1]]))
coredata(series[[1]]) <- as.numeric(coredata(series[[1]]))

library(stringr)
desc.full <- c(desc, desc2, desc3, desc4)
test <- str_subset(raw.autres2$desc, pattern = 'chômage')
