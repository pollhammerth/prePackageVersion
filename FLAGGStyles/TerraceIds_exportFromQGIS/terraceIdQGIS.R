
rm(list=ls())
setwd("/Users/tpol/TPOL/GEOLOGIE/PROJEKTE/FLAGGII/GIS/R/Analysis/PMT2.6/FLAGGStyles/TerraceIds_exportFromQGIS")

ds <- read.csv("DS_ids.csv")
head(ds)
unique(ds$NAME_KURZ)

HDS.ids <- ds[ds[1] == "06a_HDS_Donau_Biber_1800_780_ka",][["terrace"]]
hdsIds <- data.frame(HDS.ids,remarks = "")

TDS.ids <- ds[ds[1] == "03c_TDS_Mindel_Guenz_1800_780_ka",][["terrace"]]
tdsIds <- data.frame(TDS.ids,remarks = "")

ODM.ids <- ds[ds[1] == "03ba_ODM",][["terrace"]]
odmIds <- data.frame(ODM.ids,remarks = "")

ODG.ids <- ds[ds[1] == "03bb_ODG",][["terrace"]]
odgIds <- data.frame(ODG.ids,remarks = "")

ODS.ids <- ds[ds[1] == "03b_OSDS_undiff_1800_780_ka",][["terrace"]]
odsIds <- data.frame(ODS.ids,remarks = "")

dsIds <- list(hds = hdsIds, tds = tdsIds, odm = odmIds, odg = odgIds, ods = odsIds)

save(list = c("dsIds"), file = "dsIds.Rdata")

