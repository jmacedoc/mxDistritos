rm(list = ls())

############################################
# Título: Redistritación Tamaulipas        #
# Autor: Josemaría Macedo Carrillo         #
# Fecha: 14-11-17                          #
# Materia: Seminario de Investigación D    #
############################################ 

# Instalo paquetes necesarios
install.packages(c("foreign", "readxl", "tidyverse", "dplyr"))

require (foreign)
require (readxl)
require (tidyverse)
require (dplyr)

# Establezco directorios
inp <- "/Users/josemariamacedo/Desktop/ITAM/8vo semestre/Seminario D/INP"
out <- "/Users/josemariamacedo/Desktop/ITAM/8vo semestre/Seminario D/OUT"

# Abrir bases de datos
dist10 <- read_xlsx(paste(inp, "distritacion 2013 tamaulipas.xlsx", sep="/"))
dist15 <- read_xlsx(paste(inp, "distritacion 2015 tamaulipas.xlsx", sep="/"))

# Quitar variables que no nos sirven de distritacion de 2013
names(dist10)
dist10 <- dist10[,c("DISTRITO","NOMBRE MUNICIPIO","SECCION","LISTA NOMINAL")]

# Crear una nueva variable que sume lista nominal por sección
tempo10 <- group_by(dist10, SECCION)
names(tempo10)
names(tempo10)[4] <- "lista"
tempo10 <- mutate(tempo10, lista_seccion=sum(lista, na.rm=F))
tempo10 <- select(tempo10, -lista)

# Colapso por sección
tempo10 <- ungroup(tempo10)
names(tempo10)[2] <- "municipio10"
tempo10 <- group_by(tempo10, DISTRITO, municipio10, SECCION, lista_seccion)
dist10 <- summarise(tempo10)

# Ordeno ambas bases de datos por seccion de menor a mayor para ver diferencias en número de secciones
dist10 <- arrange(dist10, dist10$SECCION)
dist15 <- arrange(dist15, dist15$SECCION)

# Quito renglón innecesario
dist10 = dist10[-1905,]
View(dist10)

# Cambio nombres de variables de ambas bases para no confundirlas entre sí antes de combinarlas
names(dist10)
names(dist15)
names(dist10)[1] <- "distrito10"
names(dist10)[4] <- "lista_nom10"
names(dist15)[1] <- "distrito15"
names(dist15)[2] <- "clave_mun15"
names(dist15)[3] <- "municipio15"
names(dist15)[5] <- "poblacion15"

# Juntamos las dos bases de datos en una nueva base llamada "distritos"
distritos <- merge(x = dist10, y = dist15, by = "SECCION", all = TRUE)

# Ordenar por distrito en 2010 y 2015 aparte de sección. Aparte quitar variables de población
distritos <- arrange(distritos, distritos$distrito15, distritos$distrito10, distritos$SECCION)
distritos <- distritos[,c(-4,-8)]

# dsi visto desde la perspectiva del hijo
# "padre" de nuevo distrito e índice de similaridad de distrito (dsi), cf. Cox & Katz
distritos$father10 <- NA
distritos$dsi <- 0
for (i in 1:22){
  sel.n <- which(distritos$distrito15==i)                  # secciones en nuevo distrito
  tmp <- table(distritos$distrito10[sel.n])
  target <- as.numeric(names(tmp)[tmp==max(tmp)]) 
  distritos$father10[sel.n] <- target
  sel.f <- which(distritos$distrito10==target) # secciones en distrito padre
  sel.c <- intersect(sel.n, sel.f)             # secciones comunes entre padre y nuevo distrito
  distritos$dsi[sel.n] <- round( length(sel.c) / (length(sel.f) + length(sel.n) - length(sel.c)) , 3 )
}

#Exportamos nueva base con distrito padre y porcentaje de sección en común
write.csv(distritos, paste(out, "tam_loc.csv", sep="/"))

