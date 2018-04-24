library(geomorph)
library(geiger)
library(tictoc)

########

## Comment below via Dean C Adams, pers. comm., Sept 6, 2017:
#AGH! One issue is that the rootdata contains factors and continuous variables. That is fine, but requires some pre-processing
#before dumping these into the gdf. Otherwise, calling the columns by name is trying to call an object 'embedded' within
#another object. Won't always work.
## End pers. comm.

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
# Xvars <- c(AnnMrad+ AnnP+ AorP+ COV.Endo.D+ COV.Endo.T+ COV.Meta.A+ Hab+ Iso+ M.Endo.D+ M.Endo.T+
#      M.Meta.A+ M.Meta.D+ M2S+ MAT+ MaxTwarmW+ Mdiurnal+ MinTcoldW+ MmoistindexcoldQ+ 
#      MmoistindexdryQ+ MmoistindexwarmQ+ MmoistindexwetQ+ MTcoldQ+ MTdryQ+ MTwarmQ+ MTwetQ+
#      N.Meta.V+ PcoldQ+ PdryQ+ Pdryweek+ PhotoType+ Pseasonality+ PwarmQ+ PwetQ+ Pwetweek+
#      S.Area+ S.Dia+ Subf+ T.Meta.A+ Trange+ Tseason+N.Endo.Layers)

tic()

## Reduced models chosen by stepwise selection

M.Endo.D.pgls <- procD.pgls(M.Endo.D ~ COV.Endo.D+ COV.Endo.T+ M.Endo.T + M2S +
                              PhotoType + N.Meta.V*PwetQ,
                         phy=roottree, data=rootdata.gdf)

COV.Endo.D.pgls <- procD.pgls(COV.Endo.D ~ Pdryweek*PwetQ + M.Endo.T,
                             phy=roottree, data=rootdata.gdf)

COV.Endo.T.pgls <- procD.pgls(COV.Endo.T ~ M.Endo.D + COV.Meta.A + M.Endo.T + PdryQ,
                              phy=roottree, data=rootdata.gdf)

COV.Meta.A.pgls <- procD.pgls(COV.Meta.A ~ M.Meta.A + Subf + MaxTwarmW,
                              phy=roottree, data=rootdata.gdf)

COV.Meta.D.pgls <- procD.pgls(COV.Meta.D ~ COV.Meta.A + M.Meta.D*M2S+ Subf*S.Dia +T.Meta.A,
                              phy=roottree, data=rootdata.gdf)

M.Endo.T.pgls <- procD.pgls(M.Endo.T ~ COV.Endo.D+M.Meta.A+ MAT*Subf,
                              phy=roottree, data=rootdata.gdf)

M.Meta.A.pgls <- procD.pgls(M.Meta.A ~ M.Endo.T+ Subf*M.Meta.D,
                            phy=roottree, data=rootdata.gdf)

M.Meta.D.pgls <- procD.pgls(M.Meta.D ~ M.Meta.A + COV.Meta.D,
                            phy=roottree, data=rootdata.gdf)

M2S.pgls <- procD.pgls(M2S ~ S.Area*Subf + T.Meta.A*S.Dia,
                            phy=roottree, data=rootdata.gdf)

S.Area.pgls <- procD.pgls(S.Area ~ S.Dia*Subf,
                          phy=roottree, data=rootdata.gdf)

S.Dia.pgls <- procD.pgls(S.Dia ~ S.Area*Subf,
                          phy=roottree, data=rootdata.gdf)

T.Meta.A.pgls <- procD.pgls(T.Meta.A ~ S.Dia+ M2S+ M.Meta.A*N.Meta.V,
                         phy=roottree, data=rootdata.gdf)

N.Meta.V.pgls <- procD.pgls(N.Meta.V ~ T.Meta.A+ M.Meta.D+ M.Meta.A + COV.Meta.D + 
                              Subf*M.Endo.D+ Subf*M.Meta.D,
                            phy=roottree, data=rootdata.gdf)

N.Endo.Layers.pgls <- procD.pgls(N.Endo.Layers ~ T.Meta.A+ COV.Meta.A + AnnMrad*PhotoType,
                             phy=roottree, data=rootdata.gdf)

Med.phloem.clusters.pgls <- procD.pgls(Med.phloem.clusters ~ AnnMrad+ MmoistindexcoldQ+ 
                                         Pwetweek+ Subf*MmoistindexwetQ,
                                       phy=roottree, data=rootdata.gdf)
toc()


