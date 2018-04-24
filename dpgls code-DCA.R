library(geomorph)
library(geiger)

########

#AGH! One issue is that the rootdata contains factors and continuous variables. That is fine, but requires some pre-processing
  #before dumping these into the gdf. Otherwise, calling the columns by name is trying to call an object 'embedded' within
  #another object. Won't always work.

rootdata <- read.csv("rootdata.csv", header=TRUE)
rownames(rootdata)<-rootdata[,1]
roottree <- read.nexus("roottree.nex")
name.check(roottree, rootdata)

##Pull out columns of interest
M.area<-rootdata$Mean.Metaxylem.Area; names(M.area)<-rownames(rootdata)
Hab<-rootdata$Habitat3; names(Hab)<-rownames(rootdata)

# Convert to gdf
rootdata.gdf <- geomorph.data.frame(M.area=M.area, Hab=Hab,roottree=roottree)

test.pgls <- procD.pgls(M.area ~ Hab, phy=roottree, data=rootdata.gdf)
summary(test.pgls)
test.pgls$pgls.coefficients

# This is just to see whether the formula works elsewhere
library(nlme)  #DCA: added this library
pglsModel <- gls(Mean.Metaxylem.Area ~ Habitat3, correlation = corBrownian(phy = roottree),
                 data = rootdata, method = "ML")
# Looks okay
anova(pglsModel)  #notice F value for Habitat is the same as in D-pgls
summary(pglsModel)  #coefficients are also identical
