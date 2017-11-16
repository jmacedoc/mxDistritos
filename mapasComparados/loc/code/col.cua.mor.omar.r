rm(list = ls())

dd <- "/home/eric/Desktop/MXelsCalendGovt/redistrict/ife.ine/mapasComparados/loc"
# dd <- "/home/eric/Desktop/MXelsCalendGovt/redistrict/ife.ine/redisProcess/ineRedist2017/deJsonConEtiquetas/loc"
setwd(dd)

# Estos son los archivos que circuló omar
col2 <- read.csv("fuenteAlumnos/omarDistCol.csv", stringsAsFactors = FALSE)
cua2 <- read.csv("fuenteAlumnos/omarDistChih.csv", stringsAsFactors = FALSE)
mor2 <- read.csv("fuenteAlumnos/omarDistMor.csv", stringsAsFactors = FALSE)

# Estos son los archivos originales
col1 <- read.csv("../../redisProcess/ineRedist2017/deJsonConEtiquetas/loc/colLoc.csv", stringsAsFactors = FALSE)
cua1 <- read.csv("../../redisProcess/ineRedist2017/deJsonConEtiquetas/loc/cuaLoc.csv", stringsAsFactors = FALSE)
mor1 <- read.csv("../../redisProcess/ineRedist2017/deJsonConEtiquetas/loc/mor12Loc.csv", stringsAsFactors = FALSE)
mor18 <- read.csv("../../redisProcess/ineRedist2017/deJsonConEtiquetas/loc/mor18Loc.csv", stringsAsFactors = FALSE) # también incorporaré el mapa con 18 distritos

head(col2)
dim(cua2)
dim(cua1)



## # chihuahua
## colnames(cua1)
## cua1 <- cua1[,c("edon","seccion","munn","escenario3")]
## colnames(cua1) <- c("edon","seccion","munn","disn2018")
## 
## # cambia nombres en datos de Omar
## head(cua2)
## table(cua2$merge)
## cua2$merge <- NULL
## colnames(cua2) <- c("disn2015","seccion","disn2013") # omar: investiga el año electoral inaugural del mapa abandonado (será el nombre definitivo) 
## 
## # fusiona
## cua <- merge(x = cua1, y = cua2, by = "seccion", all = TRUE)
## 
## dim(cua)
## dim(cua1)
## dim(cua2)
## 
## head(cua)
## table(cua$disn2015, cua$disn2018) # disn mismatch, me quedo con el núm de omar
## cua$disn2018 <- cua$disn2015; cua$disn2015 <- NULL
## 
## write.csv(cua, file = "fuenteAlumnos/cuaLoc.csv", row.names = FALSE) 




## # colima
## colnames(col1)
## col1 <- col1[,c("edon","seccion","munn","escenario3")]
## colnames(col1) <- c("edon","seccion","munn","disn2018")
## 
## # cambia nombres en datos de Omar
## head(col2)
## table(col2$X_merge)
## col2$X_merge <- NULL
## colnames(col2) <- c("disn2016","seccion","disn2014") # omar: investiga el año electoral inaugural del mapa abandonado (será el nombre definitivo) 
## 
## # fusiona
## col <- merge(x = col1, y = col2, by = "seccion", all = TRUE)
## 
## dim(col)
## dim(col1)
## dim(col2)
## 
## head(col)
## 
## table(col$disn2016, col$disn2018) # disn mismatch, me quedo con el núm de omar
## col$disn2018 <- col$disn2016; col$disn2016 <- NULL
## 
## write.csv(col, file = "fuenteAlumnos/colLoc.csv", row.names = FALSE)




# morelos
colnames(mor1)
mor1 <- mor1[,c("edon","seccion","munn","escenario3")]
colnames(mor1) <- c("edon","seccion","munn","disn2018")

# cambia nombres en datos de Omar
head(mor2)
mor2$X_merge <- NULL
colnames(mor2) <- c("disn2017","seccion","disn2012") # omar: investiga el año electoral inaugural del mapa abandonado (será el nombre definitivo) 

# fusiona
mor <- merge(x = mor1, y = mor2, by = "seccion", all = TRUE)

dim(mor)
dim(mor1)
dim(mor2)

head(mor)

table(mor$disn2018, mor$disn2017) # disn mismatch, me quedo con el núm de omar
cua$disn2018 <- cua$disn2017; cua$disn2017 <- NULL

#write.csv(mor, file = "fuenteAlumnos/mor12Loc.csv", row.names = FALSE)




## # cua
## ## READ HISTORICAL MAP (MISSING SECCIONES POSSIBLE)
## d <- read.csv(file = "fuenteAlumnos/cuaLoc.csv", stringsAsFactors = FALSE)
## head(d) 
## # handy function to rename one data.frame's column
## rename.col <- function(old=NA, new=NA, what=NA){
##     old <- old; new <- new; what <- what;
##     colnames(what)[which(colnames(what)==old)] <- new
##     return(what)
## }
## d <- rename.col(old="disn2013", new="disloc2013", what=d)
## d <- rename.col(old="disn2018", new="disloc2018", what=d)
## #
## # ---> NOTE:                                                                         <--- #
## # ---> open useEqPrep2fillMissSeccionesLocalMaps.r and run manually to spot errors   <--- #
## # ---> will generate new eq object with full map (incl. state and federal districts) <--- #
## 
## write.csv(eq, file = "cuaLoc.csv", row.names = FALSE)


## # col
## ## READ HISTORICAL MAP (MISSING SECCIONES POSSIBLE)
## d <- read.csv(file = "fuenteAlumnos/colLoc.csv", stringsAsFactors = FALSE)
## head(d) # dist_old year needed
## 
## # handy function to rename one data.frame's column
## rename.col <- function(old=NA, new=NA, what=NA){
##     old <- old; new <- new; what <- what;
##     colnames(what)[which(colnames(what)==old)] <- new
##     return(what)
## }
## d <- rename.col(old="disn2014", new="disloc2014", what=d)
## d <- rename.col(old="disn2018", new="disloc2018", what=d)
## #
## # ---> NOTE:                                                                         <--- #
## # ---> open useEqPrep2fillMissSeccionesLocalMaps.r and run manually to spot errors   <--- #
## # ---> will generate new eq object with full map (incl. state and federal districts) <--- #
## 
## write.csv(eq, file = "colLoc.csv", row.names = FALSE)

