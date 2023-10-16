# lists of local names and associated terrace ids of DS polygons:

hdsNames <- list(
  Salen_Reutenen = 5906,
  Schiener_Berg = c(7017,6962,1303),
  Neuhauser_Wald = 1264,
  Irchel = 1278,
  Stadlerberg = 1281,
  Egg = 1280,
  Bowald = c(1277,1279),
  Acheberg = c(1267),#,1271),
  Buechli = 1271,
  Lachen = 1269,
  Berchenwald = c(1266,1265),
  Tannboden = 18838,
  Kegelplatz = c(18798, 18828, 18827),
  Duern_Glaend = 1283,
  Mandach = c(1298,1299, 1302,1301), # 1302, 1301 sind redundante Polygone!
  MandachN = 1274,
  SELaegern = c(1286,1287,1288),
  Heitersberg = 1289,
  EZuerichsee = c(1292, 1294, 1295, 1296),
  WReuss = c(1290,1291,1293, 1297),
  Dinkelberg = c(1268, 1270,1272,1273,1275,1276),
  Berg = c(1285,1300,1284,1282)
)

# Index 1 = TDS hoeheres Niveau, 2 = TDS tieferes Niveau nach Graf (2009), MDS = Mittlere DS nach Graf (1993)
tdsNames <- list(
  Homberg_ODM = c(7408,7332,7440,7380,7341,7429),
  Heiligenberg_ODG = c(7430,7439),
  Fuerstenberg_ODM = 7277,
  Hummelberg = 6003,
  Sipplingen_1 = c(7369,7262),
  Sipplingen_2 = c(7270),
  Lerchenacker_1 = c(7423,7271),
  Hochreute_1 = c(7372),
  MuehlbergNord_2 = 7335,
  Hinterhomburg_2 = c(6960,6971),
  Rossberg_2 = c(7021),
  FriedingerSchlossberg_2 = 6966,
  Heilsberg_2 = 1257,
  Buechberg_2 = c(1258,1165),
  BergBeiThayngen_2 = 1256,
  Hochfluh_2 = c(1255,1254),
  Geissberg_2 = c(1166,1167),
  KlettgauEingang_2 = 1168,
  HasenbergWilchingen = c(1171,1172),
  Rechberg = c(1174),
  Wutach = c(18822),
  SchienerBerg_1 = c(6974,6969,6999,7015,6973,6963,1243,1247),
  SchienerBerg_2 = c(1244,1245,1248,1249,1251),
  SchienerBerg_1u = c(6961,1242),
  Rauhenberg_1 = c(1252,1250),
  HeerenbergHoernliwald_1 = c(6001,1259),
  Stammergberg_1 = 1241,
  Cholfirst_2 = 1170,
  Hoernli = c(1263,1304),
  SELaegern = c(1220,1219,1221,1218),
  WReuss = c(1231,1230,1229,1227,1222,1223,1224,1226,1225,1228),
  Irchel = c(1193,1185,1182),
  Rhinsberg = 1183,
  Laubberg = 1181,
  Aemperg = 1186,
  Sanzenberg = c(1189,1191),
  SanzenbergSW = c(1194,1197),
  Belchen = c(1187,1240),
  BelchenE = c(1192,1190),
  Dangstetten = c(1178,1180),#1176),
  Berchenwald = 1176,
  Buechli = c(1184,1239,1237,1238),
  Firsthalden = 1196,
  Iberig = 1199,
  Bruggerberg = 1213,
  GebenstorferHorn_MDS = 1214,
  Langholz_MDS = c(1217,1215),
  Baregg = 1216,
  Ifang = c(1179,1188,1233,1232, 1236,1235), # 1236, 1235 sind redundante Polygone!
  Muehleberg = c(18846,18837,18829),
  Aarberg = 1175,
  WAarberg = 18848,
  Wehrtal = c(18685,18716),
  Dinkelberg = c(18756,18684,1177,1260,1261,1262),
  Berg = c(1195,1234,1204,1202,1200,1201,1209,1211,1207,1203,1205,1208,1206,1210,1212, 1198)
)


# set upper level of TDS acc to Graf (2009)
tdshNames <- c(tdsNames$Sipplingen_1, tdsNames$Lerchenacker_1, tdsNames$Hochreute_1, 
 tdsNames$SchienerBerg_1, tdsNames$SchienerBerg_1u, tdsNames$Rauhenberg_1, 
 tdsNames$HeerenbergHoernliwald_1, tdsNames$Stammergberg_1) # 6973 # terrace id "Schrotzburg", N Hang Schienerberg, steiles Gelaende, kleines Polygon
# set lower level of TDS acc to Graf (2009)  
tdstNames <- c(tdsNames$Sipplingen_2, tdsNames$MuehlbergNord_2, tdsNames$Hinterhomburg_2, tdsNames$Rossberg_2, 
               tdsNames$FriedingerSchlossberg_2, tdsNames$Heilsberg_2, tdsNames$Buechberg_2, tdsNames$BergBeiThayngen_2, 
               tdsNames$Hochfluh_2, tdsNames$Geissberg_2, tdsNames$KlettgauEingang_2, tdsNames$SchienerBerg_2, 
               tdsNames$Cholfirst_2, tdsNames$Aarberg, tdsNames$Rechberg, tdsNames$HasenbergWilchingen) 


#### TDS by region
tdsRegion <- list(
  # Oberschwaben Deckenschotter
    ObeschwabenDS = c(tdsNames$Homberg_ODM, tdsNames$Heiligenberg_ODG, tdsNames$Fuerstenberg_ODM),
  # Bodensee N (Sipplingen bis Klettgau Eingang)
    BodenseeN = c(tdsNames$Sipplingen_1, tdsNames$Sipplingen_2, tdsNames$Lerchenacker_1, tdsNames$Hochreute_1,
        tdsNames$Hinterhomburg_2, tdsNames$Rossberg_2, tdsNames$Heilsberg_2, tdsNames$Buechberg_2,
        tdsNames$BergBeiThayngen_2, tdsNames$Hochfluh_2, tdsNames$Geissberg_2, tdsNames$KlettgauEingang_2),
  # Bodensee S (Schienerberg bis Cholfirst)
     BodenseeS = c(tdsNames$SchienerBerg_1, tdsNames$SchienerBerg_1u, tdsNames$SchienerBerg_2, tdsNames$Rauhenberg_1,
         tdsNames$HeerenbergHoernliwald_1, tdsNames$Stammergberg_1, tdsNames$Cholfirst_2),
  # Klettgau
    Klettgau = c(tdsNames$HasenbergWilchingen, tdsNames$Rechberg),
  # Aare Rhein Konfluenz
    AareRheinKonfluenz = c(tdsNames$Aarberg, tdsNames$WAarberg),
  # Aaretal
    Aaretal = c(tdsNames$Ifang, tdsNames$Langholz_MDS, tdsNames$GebenstorferHorn_MDS, tdsNames$Iberig,
        tdsNames$Firsthalden, tdsNames$Bruggerberg, tdsNames$Baregg, tdsNames$Buechli),
  # Rheintal suedlich Klettgau
    RheintalSKlettgau = c(tdsNames$Dangstetten, tdsNames$Belchen, tdsNames$BelchenE, tdsNames$SanzenbergSW,
        tdsNames$Sanzenberg, tdsNames$Aemperg, tdsNames$Laubberg, tdsNames$Rhinsberg, tdsNames$Irchel, tdsNames$Berchenwald),
  # distant to Rhine, connection unclear
    Distant = c(tdsNames$WReuss, tdsNames$SELaegern, tdsNames$Hoernli),
  # Wutach
    Wutach = c(tdsNames$Wutach),
  # richtung Basel
    Basel = c(tdsNames$Wehrtal, tdsNames$Dinkelberg, tdsNames$Berg),
  # extreme positions
    Extreme = c(tdsNames$FriedingerSchlossberg_2, tdsNames$MuehlbergNord_2, tdsNames$Muehleberg, tdsNames$Hummelberg)
)
