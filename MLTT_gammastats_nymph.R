#nymphalids

library(ape)
library(laser)
nymph_dna=read.nexus(file.choose())
nymph_pastis=read.nexus(file.choose())

#scale trees to unit depth
nymph2 <- nymph_dna

for (i in 1:length(nymph_dna)){
  nymph2[[i]]<-rescaleTree(nymph_dna[[i]], 10)
}

mltt.plot(nymph2, dcol = TRUE, dlty = FALSE, legend = FALSE, xlab = "Time", ylab = "N", log = "y", backward = TRUE)

nymph3 <- nymph_pastis

for (i in 1:length(nymph_pastis)){
  nymph3[[i]]<-rescaleTree(nymph_pastis[[i]], 10)
}

mltt.plot(nymph3, dcol = TRUE, dlty = FALSE, legend = FALSE, xlab = "Time", ylab = "N", log = "y", backward = TRUE)


#LTT
mltt.plot(nymph_pastis, dcol = TRUE, dlty = FALSE, legend = FALSE, xlab = "Time", ylab = "N", log = "y", backward = TRUE)
quartz()
mltt.plot(nymph_dna, dcol = TRUE, dlty = FALSE, legend = FALSE, xlab = "Time", ylab = "N", log = "y", backward = TRUE)


#gammastat

matrix(NA, 1, length(nymph_dna)) -> dnaGamma

for (i in 1:length(nymph_dna)){gammaStat(nymph_dna[[i]]) -> dnaGamma[1,i]}

hist(dnaGamma)

max(dnaGamma[1,])

mccrTest(114, 24, 100, max(dnaGamma[1,])) #definitely looks straight
mccrTest(114, 24, 100, median(dnaGamma[1,])) #might have a slowdown


matrix(NA, 1, length(nymph_pastis)) -> pastisGamma

for (i in 1:length(nymph_pastis)){gammaStat(nymph_pastis[[i]]) -> pastisGamma[1,i]}

hist(pastisGamma)

mccrTest(114, 0, 100, max(pastisGamma[1,])) #definitely looks straight
mccrTest(114, 0, 100, median(pastisGamma[1,])) #definitely looks straight

