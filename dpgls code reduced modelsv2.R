
##############################################################
#  Credit for the ipak function goes to Steven Worthington:  #
#  https://gist.github.com/stevenworthington/3178163         #
##############################################################

# Function to check the local installation for a list of packages, install any that are missing, and load into memory.
ipak <- function(pkg){
    new.pkg <- pkg[!(pkg %in% installed.packages()[, "Package"])]
    if (length(new.pkg)) 
        install.packages(new.pkg, dependencies = TRUE)
    sapply(pkg, require, character.only = TRUE)
}

# List of packages required
packages <- c("geomorph", "geiger", "FactoMineR", "flextable", "pca3d", "ggplot2")

# Run load.pkgs function on the list of required packages
ipak(packages)

####################################################

## Comment below via Dean C Adams, pers. comm., Sept 6, 2017:
# One issue is that the rootdata contains factors and continuous variables. That is fine, but requires some pre-processing
#before dumping these into the gdf. Otherwise, calling the columns by name is trying to call an object 'embedded' within
#another object. Won't always work.
## End pers. comm.

###################################################

# Setup
rootdata <- read.csv("rootdata.csv", header=TRUE)
rownames(rootdata) <- rootdata$Taxon
roottree <- read.nexus("roottree.nex")
name.check(roottree, rootdata)


# Call all columns of interest (this is what comment from DCA above refers to):
AorP <- rootdata$Annual_Perennial2; names(AorP)<-rownames(rootdata)
Hab<-rootdata$Habitat3; names(Hab)<-rownames(rootdata)
Subf<-rootdata$Subfamily; names(Subf)<-rownames(rootdata)
PhotoType<-rootdata$Photosynthetic_Type; names(PhotoType)<-rownames(rootdata)
S.Dia<-rootdata$Stele.Diameter; names(S.Dia)<-rownames(rootdata)
S.Area<-rootdata$Stele.Area; names(S.Area)<-rownames(rootdata)
N.Meta.V<-rootdata$No.Primary.Metaxylem.Vessels; names(N.Meta.V)<-rownames(rootdata)
M.Meta.D<-rootdata$Mean.Metaxylem.Diameter; names(M.Meta.D)<-rownames(rootdata)
M.Meta.A<-rootdata$Mean.Metaxylem.Area; names(M.Meta.A)<-rownames(rootdata)
M.Endo.T<-rootdata$Mean.Endodermal.Thickening; names(M.Endo.T)<-rownames(rootdata)
M.Endo.D<-rootdata$Mean.Endodermal.Cell.Depth; names(M.Endo.D)<-rownames(rootdata)
SD.Meta.D<-rootdata$SD.Metaxylem.Diameter; names(SD.Meta.D)<-rownames(rootdata)
SD.Meta.A<-rootdata$SD.Metaxylem.Area; names(SD.Meta.A)<-rownames(rootdata)
SD.Endo.T<-rootdata$SD.Endodermal.Thickening; names(SD.Endo.T)<-rownames(rootdata)
SD.Endo.D<-rootdata$SD.Endodermal.Cell.Depth; names(SD.Endo.D)<-rownames(rootdata)
COV.Meta.D<-rootdata$COV.Metaxylem.Diameter; names(COV.Meta.D)<-rownames(rootdata)
COV.Meta.A<-rootdata$COV.Metaxylem.Area; names(COV.Meta.A)<-rownames(rootdata)
COV.Endo.T<-rootdata$COV.Endodermal.Thickening; names(COV.Endo.T)<-rownames(rootdata)
COV.Endo.D<-rootdata$COV.Endodermal.Cell.Depth; names(COV.Endo.D)<-rownames(rootdata)
T.Meta.A<-rootdata$Total.metaxylem.area; names(T.Meta.A)<-rownames(rootdata)
M2S<-rootdata$Metaxylem.to.Stele.Ratio; names(M2S)<-rownames(rootdata)
MAT<-rootdata$MAT.degC; names(MAT)<-rownames(rootdata)
Mdiurnal<-rootdata$Mdiurnalrange.degC; names(Mdiurnal)<-rownames(rootdata)
Iso<-rootdata$Isothermality; names(Iso)<-rownames(rootdata)
Tseason<-rootdata$Tseasonality; names(Tseason)<-rownames(rootdata)
MaxTwarmW<-rootdata$MaxTwarmweek.degC; names(MaxTwarmW)<-rownames(rootdata)
MinTcoldW<-rootdata$MinTcoldweek.degC; names(MinTcoldW)<-rownames(rootdata)
Trange<-rootdata$Tannrange.degC; names(Trange)<-rownames(rootdata)
MTwetQ<-rootdata$MtempwetQ.degC; names(MTwetQ)<-rownames(rootdata)
MTdryQ<-rootdata$MtempdryQ.degC; names(MTdryQ)<-rownames(rootdata)
MTwarmQ<-rootdata$MtempwarmQ.degC; names(MTwarmQ)<-rownames(rootdata)
MTcoldQ<-rootdata$MtempcoldQ.degC; names(MTcoldQ)<-rownames(rootdata)
AnnP<-rootdata$AnnP.mm; names(AnnP)<-rownames(rootdata)
Pwetweek<-rootdata$Pwetweek.mm; names(Pwetweek)<-rownames(rootdata)
Pdryweek<-rootdata$Pdryweek.mm; names(Pdryweek)<-rownames(rootdata)
Pseasonality<-rootdata$Pseasonality; names(Pseasonality)<-rownames(rootdata)
PwetQ<-rootdata$PwetQ.mm; names(PwetQ)<-rownames(rootdata)
PdryQ<-rootdata$PdryQ.mm; names(PdryQ)<-rownames(rootdata)
PwarmQ<-rootdata$PwarmQ.mm; names(PwarmQ)<-rownames(rootdata)
PcoldQ<-rootdata$PcoldQ.mm; names(PcoldQ)<-rownames(rootdata)
AnnMrad<-rootdata$Annmeanradiation.Wm2; names(AnnMrad)<-rownames(rootdata)
MmoistindexwetQ<-rootdata$MmoistureindexwetQ; names(MmoistindexwetQ)<-rownames(rootdata)
MmoistindexdryQ<-rootdata$MmoistureindexdryQ; names(MmoistindexdryQ)<-rownames(rootdata)
MmoistindexwarmQ<-rootdata$MmoistureindexwarmQ; names(MmoistindexwarmQ)<-rownames(rootdata)
MmoistindexcoldQ<-rootdata$MmoistureindexcoldQ; names(MmoistindexcoldQ)<-rownames(rootdata)
N.Endo.Layers<-rootdata$Number.Endodermal.Layers; names(N.Endo.Layers)<-rownames(rootdata)
Med.phloem.clusters<-rootdata$Median.phloem.clusters; names(Med.phloem.clusters)<-rownames(rootdata)
Genus<-rootdata$Genus; names(Genus)<-rownames(rootdata)

# Convert to gdf
rootdata.gdf <- geomorph.data.frame(AnnMrad=AnnMrad,AnnP=AnnP,AorP=AorP,COV.Endo.D=COV.Endo.D,
                  COV.Endo.T=COV.Endo.T, COV.Meta.A=COV.Meta.A,COV.Meta.D=COV.Meta.D,
                  Hab=Hab,Iso=Iso,M.Endo.D=M.Endo.D,M.Endo.T=M.Endo.T,
                  M.Meta.A=M.Meta.A,M.Meta.D=M.Meta.D,M2S=M2S,MAT=MAT,MaxTwarmW=MaxTwarmW,
                  Mdiurnal=Mdiurnal,MinTcoldW=MinTcoldW,MmoistindexcoldQ=MmoistindexcoldQ,
                  MmoistindexdryQ=MmoistindexdryQ,MmoistindexwarmQ=MmoistindexwarmQ,
                  MmoistindexwetQ=MmoistindexwetQ,MTcoldQ=MTcoldQ,MTdryQ=MTdryQ,MTwarmQ=MTwarmQ,
                  MTwetQ=MTwetQ,N.Meta.V=N.Meta.V,PcoldQ=PcoldQ,PdryQ=PdryQ,Pdryweek=Pdryweek,
                  PhotoType=PhotoType,Pseasonality=Pseasonality,PwarmQ=PwarmQ,PwetQ=PwetQ,Pwetweek=Pwetweek,
                  S.Area=S.Area,S.Dia=S.Dia,SD.Endo.D=SD.Endo.D,SD.Endo.T=SD.Endo.T,SD.Meta.A=SD.Meta.A,
                  SD.Meta.D=SD.Meta.D,Subf=Subf,T.Meta.A=T.Meta.A,Trange=Trange,Tseason=Tseason,
                  N.Endo.Layers=N.Endo.Layers, Med.phloem.clusters=Med.phloem.clusters, Genus=Genus,
                  roottree=roottree)

# Reduced models chosen by stepwise selection

M.Endo.D.pgls <- procD.pgls(M.Endo.D ~ COV.Endo.D+ COV.Endo.T+ M.Endo.T + M2S +
                              PhotoType + N.Meta.V*PwetQ,
                         phy=roottree, data=rootdata.gdf)

COV.Endo.D.pgls <- procD.pgls(COV.Endo.D ~ M.Endo.D + Pdryweek*PwetQ + M.Endo.T,
                             phy=roottree, data=rootdata.gdf)

COV.Endo.T.pgls <- procD.pgls(COV.Endo.T ~ M.Endo.D + M.Endo.T + COV.Meta.D + COV.Meta.A,
                              phy=roottree, data=rootdata.gdf)

COV.Meta.A.pgls <- procD.pgls(COV.Meta.A ~ M.Meta.A + MaxTwarmW + N.Meta.V,
                              phy=roottree, data=rootdata.gdf)

COV.Meta.D.pgls <- procD.pgls(COV.Meta.D ~ COV.Meta.A,
                              phy=roottree, data=rootdata.gdf)

M.Endo.T.pgls <- procD.pgls(M.Endo.T ~ COV.Endo.D + M.Meta.A + MAT*Subf,
                              phy=roottree, data=rootdata.gdf)

M.Meta.A.pgls <- procD.pgls(M.Meta.A ~ M.Endo.T+ Subf*M.Meta.D,
                            phy=roottree, data=rootdata.gdf)

M.Meta.D.pgls <- procD.pgls(M.Meta.D ~ M.Meta.A*Subf,
                            phy=roottree, data=rootdata.gdf)

M2S.pgls <- procD.pgls(M2S ~ log(S.Area)*Subf + T.Meta.A*log(S.Dia),
                            phy=roottree, data=rootdata.gdf)

S.Area.pgls <- procD.pgls(log(S.Area) ~ N.Endo.Layers + M2S + log(S.Dia),
                          phy=roottree, data=rootdata.gdf)

S.Dia.pgls <- procD.pgls(log(S.Dia) ~ N.Endo.Layers*Subf + T.Meta.A,
                          phy=roottree, data=rootdata.gdf)

T.Meta.A.pgls <- procD.pgls(T.Meta.A ~ log(S.Dia)+ M2S+ M.Meta.A*N.Meta.V,
                         phy=roottree, data=rootdata.gdf)

N.Meta.V.pgls <- procD.pgls(N.Meta.V ~ T.Meta.A+ M.Meta.D+ M.Meta.A + 
                              Subf*M.Endo.D+ Subf*M.Meta.D,
                            phy=roottree, data=rootdata.gdf)

N.Endo.Layers.pgls <- procD.pgls(N.Endo.Layers ~ T.Meta.A+ COV.Meta.A + AnnMrad*PhotoType,
                             phy=roottree, data=rootdata.gdf)

Med.phloem.clusters.pgls <- procD.pgls(Med.phloem.clusters ~ AnnMrad+ MmoistindexcoldQ+ 
                                         Pwetweek+ Subf*MmoistindexwetQ,
                                       phy=roottree, data=rootdata.gdf)


# Sink all output to file rather than the console:

# pgls.output <- file("Preliminary pgls results ver5.csv", open = "wt")
# sink(file = pgls.output, append = TRUE, type = c("output"),
#     split = FALSE)

# Summary of each model
summary(M.Endo.D.pgls)
summary(COV.Endo.D.pgls)
summary(M.Endo.T.pgls)
summary(COV.Endo.T.pgls)
summary(N.Endo.Layers.pgls)

summary(S.Area.pgls)
summary(S.Dia.pgls)
summary(M.Meta.A.pgls)
summary(COV.Meta.A.pgls)
summary(M.Meta.D.pgls)
summary(COV.Meta.D.pgls)
summary(T.Meta.A.pgls)
summary(N.Meta.V.pgls)
summary(M2S.pgls)
summary(Med.phloem.clusters.pgls)

# End sink
# sink()

################## Supplementary table of data is based on RAW_DATA_CLEAN2.csv
##### rawdata.long is as above

# write.table(rawdata.long, file="Table_s1.csv", sep=",", col.names = TRUE, row.names=FALSE)
# write.table(rawdata.summary, file="Table_s2.csv", sep=",", col.names = TRUE, row.names=FALSE)


rootdata.for.pca <- rootdata[c(9:15, 20:25, 66:67)]
pca2 <- PCA(rootdata.for.pca, graph=TRUE)
eig <- pca2$eig
eigdf <- as.data.frame(eig)
eigdf$comp <- rownames(eigdf)

write.table(pca2$eig, file="rootdata PCA.csv", sep=",", col.names = TRUE, row.names=TRUE)
plot(pca2)


eigdf$comp <- factor(eigdf$comp, levels = eigdf$comp)
eigplot <- ggplot(data=eigdf, aes(x=comp, y=eigenvalue)) + 
  geom_line(data=eigdf, aes(group="character"), size=1.25, color = "gray") +
  geom_point(size=3) + 
  theme_minimal(base_size=14) +
  xlab("Component") + ylab("Eigenvalue") +
    theme(
      axis.title.x = element_text(face="bold"),
      axis.title.y = element_text(face="bold"))
ggsave("eigplot_lowres.png", dpi=300, scale=2)  

# Groups for PCA 3D

pca3 <- prcomp(rootdata.for.pca, scale. = TRUE)
plot(pca3)
annper <- factor(rootdata[,5])
hab <- factor(rootdata[,6])
subfamily <- factor(rootdata[,7])
ptype <- factor(rootdata[,8])

rownames(rootdata.for.pca)

pca.colors 

pcal <- pca2d(pca3, group=subfamily)
pcal$colors
plot(pca3.2d)
pca3.2d <- pca2d(pca3, gr=subfamily, 
                 biplot = TRUE, show.ellipses = FALSE, show.centroids = TRUE)
pca3.2d$colors <- c("#a6cee3", "#1f78b4", "#b2df8a", "#33a02c", "#fb9a99", "#e31a1c", "#fdbf6f", 
                "#ff7f00", "#cab2d6", "#6a3d9a", "#ffff99")
legend("right", pca3.2d$groups, pch=pca3.2d$pch,
                                            col=pca3.2d$colors, box.lty=0)