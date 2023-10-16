################################################################################
#### preset filter-expressions for terrace data for different maps #############
################################################################################




# for map: Terraces.gpkg, field: NAME_KURZ
NT <- expression(
  pmt.filter(data, 
             regard.column = "terraces.NAME_KURZ", regard.elements = "01_NT", 
             disregard.column = NA, disregard.elements = NA, 
             thresh.column = c("slope","distance"), threshold = c(max.slope,max.distance), 
             x = "location", y = y.source, z = "distance", drop.unused = FALSE))
LM <- expression(
  pmt.filter(data, 
             regard.column = "terraces.NAME_KURZ", regard.elements = c("01d_Spaet_bis_Mid_Plei","00_Spaet_bis_Postglazial","01a_NT_HT_aelter_fraglich"), 
             disregard.column = NA, disregard.elements = NA, 
             thresh.column = c("slope","distance"), threshold = c(max.slope,max.distance), 
             x = "location", y = y.source, z = "distance", drop.unused = FALSE))
HT <- expression(
  pmt.filter(data, 
             regard.column = "terraces.NAME_KURZ", regard.elements = "02_HT", 
             disregard.column = NA, disregard.elements = NA, 
             thresh.column = c("slope","distance"), threshold = c(max.slope,max.distance), 
             x = "location", y = y.source, z = "distance", drop.unused = FALSE))
JDS <- expression(
  pmt.filter(data, 
             regard.column = "terraces.NAME_KURZ", regard.elements = c("03_JDS","03ba_ODM"), 
             disregard.column = NA, disregard.elements = NA, 
             thresh.column = c("slope","distance"), threshold = c(max.slope,max.distance), 
             x = "location", y = y.source, z = "distance", drop.unused = FALSE))
MDS <- expression(
  pmt.filter(data, 
             regard.column = "terraces.NAME_KURZ", regard.elements = c("04_MDS"), 
             disregard.column = NA, disregard.elements = NA, 
             thresh.column = c("slope","distance"), threshold = c(max.slope,max.distance), 
             x = "location", y = y.source, z = "distance", drop.unused = FALSE))
MP <- expression(
  pmt.filter(data, 
             regard.column = "terraces.NAME_KURZ", regard.elements = c("02a_Mid_Plei_HT_bis_ADS","03a_JDS_ADS_loessbedeckt_undiff","08_Aelterer_Terrscho_Mid_Plei"), 
             disregard.column = NA, disregard.elements = NA, 
             thresh.column = c("slope","distance"), threshold = c(max.slope,max.distance), 
             x = "location", y = y.source, z = "distance", drop.unused = FALSE))
ADS <- expression(
  pmt.filter(data, 
             regard.column = "terraces.NAME_KURZ", regard.elements = c("05_TADS","03bb_ODG"), 
             disregard.column = NA, disregard.elements = NA, 
             thresh.column = c("slope","distance"), threshold = c(max.slope,max.distance), 
             x = "location", y = y.source, z = "distance", drop.unused = FALSE))
HADS <- expression(
  pmt.filter(data, 
             regard.column = "terraces.NAME_KURZ", regard.elements = c("06_HADS","03bc_ODD"), 
             disregard.column = NA, disregard.elements = NA, 
             thresh.column = c("slope","distance"), threshold = c(max.slope,max.distance), 
             x = "location", y = y.source, z = "distance", drop.unused = FALSE))
AADS <- expression(
  pmt.filter(data, 
             regard.column = "terraces.NAME_KURZ", regard.elements = c("07_Biber"), 
             disregard.column = NA, disregard.elements = NA, 
             thresh.column = c("slope","distance"), threshold = c(max.slope,max.distance), 
             x = "location", y = y.source, z = "distance", drop.unused = FALSE))
OD <- expression(
  pmt.filter(data, 
             regard.column = "terraces.NAME_KURZ", regard.elements = c("03b_OSDS_undiff_1800_780_ka"), 
             disregard.column = NA, disregard.elements = NA, 
             thresh.column = c("slope","distance"), threshold = c(max.slope,max.distance), 
             x = "location", y = y.source, z = "distance", drop.unused = FALSE))
HS <- expression(
  pmt.filter(data, 
             regard.column = "terraces.NAME_KURZ", regard.elements = c("10a_Hoehenschotter"), 
             disregard.column = NA, disregard.elements = NA, 
             thresh.column = c("slope","distance"), threshold = c(max.slope,max.distance), 
             x = "location", y = y.source, z = "distance", drop.unused = FALSE))
AP <- expression(
  pmt.filter(data, 
             regard.column = "terraces.NAME_KURZ", regard.elements = c("01b_Plei_Plio_undiff","11_Aeltere_TerrScho_Plio","06b_Altplei_Reu_Schn_Aich_Federn_Geier_u_m","09_Aelterer_Flussschotter_Altpleisto_bis_Juengere_","10_Altplei_Plio"), 
             disregard.column = NA, disregard.elements = NA, 
             thresh.column = c("slope","distance"), threshold = c(max.slope,max.distance), 
             x = "location", y = y.source, z = "distance", drop.unused = FALSE))
PL <- expression(
  pmt.filter(data, 
             regard.column = "terraces.NAME_KURZ", regard.elements = c("01b_Plei_undiff"), 
             disregard.column = NA, disregard.elements = NA, 
             thresh.column = c("slope","distance"), threshold = c(max.slope,max.distance), 
             x = "location", y = y.source, z = "distance", drop.unused = FALSE))
TDS <- expression(expression, 
                  pmt.filter(data, 
                             regard.column = "terraces.terrace", 
                             regard.elements = c(dsIds$tds[[1]], if (is.na(tds.add) == FALSE) {tds.add} else {NULL}), 
                             disregard.column = "terraces.terrace", 
                             disregard.elements = tds.remove, 
                             thresh.column = c("slope", "distance"), 
                             threshold = c(max.slope, max.distance), 
                             x = "location", y = y.source, z = "distance", 
                             drop.unused = FALSE))
HDS <- expression(expression, 
                  pmt.filter(data, 
                             regard.column = "terraces.terrace", 
                             regard.elements = c(dsIds$hds[[1]], if (is.na(hds.add) == FALSE) {hds.add} else {NULL}), 
                             disregard.column = "terraces.terrace", 
                             disregard.elements = hds.remove, 
                             thresh.column = c("slope", "distance"), 
                             threshold = c(max.slope, max.distance), 
                             x = "location", y = y.source, z = "distance", 
                             drop.unused = FALSE))
DEM <- expression(
  pmt.filter(data, 
             regard.column = NA, regard.elements = NA, 
             disregard.column = NA, disregard.elements = NA, 
             thresh.column = c("slope","distance"), threshold = c(max.slope,max.distance), 
             x = "location", y = y.source, z = "distance", drop.unused = FALSE))






# for standardised manual quick map
ROI <- expression(
  pmt.filter(data, 
             regard.column = "manual.NAME", regard.elements = "roi", 
             disregard.column = NA, disregard.elements = NA, 
             thresh.column = c("slope","distance"), threshold = c(max.slope,max.distance), 
             x = "location", y = y.source, z = "distance", drop.unused = FALSE))



