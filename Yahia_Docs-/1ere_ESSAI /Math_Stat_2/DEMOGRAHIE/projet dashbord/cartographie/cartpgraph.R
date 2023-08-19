library("maptools")
library(sp)
library(reshape2)
library(rgeos)
library(shapefiles)
fdc <- readShapePoly("Tunisie_snuts4")
donnees <- read.csv("tunisie_data_del_2011_2.csv", header = TRUE, sep = ";",dec = ",", encoding = "latin1")
dim(donnees)
pt <- cbind(fdc@data[, "id"], as.data.frame(coordinates(fdc)))
colnames(pt) <- c("id", "x", "y")
i=match(pt[, "id"], donnees[, "del"])
pt <- data.frame(pt, donnees[i, ])
pt$var <- pt$POPTO2010
x1 <- bbox(fdc)[1]
y1 <- bbox(fdc)[2]
x2 <- bbox(fdc)[3]
y2 <- bbox(fdc)[4]
sfdc <- (x2 - x1) * (y2 - y1)
sc <- sum(pt$var, na.rm = TRUE)
k <- 0.2  
pt$size <- sqrt((pt$var*k*sfdc/sc)/pi)
plot(fdc, border = "white", col = "grey")
symbols(pt[, c("x","y")], circles = pt$size, add = TRUE, bg = "red", inches = FALSE)

LegTitle <- "Nombre \nd'habitants\n"
rLeg <- quantile(pt$size, c(1, 0.9, 0.25, 0), type = 1, na.rm = TRUE);rLeg
rVal <- quantile(pt$var, c(1, 0.9, 0.25, 0), type = 1, na.rm = TRUE);rVal
l <- data.frame(x = x1, y = y1);head(l)
xinit <- l$x + rLeg[1];xinit
ypos <- l$y + rLeg;ypos
symbols(x = rep(xinit, 4), y = ypos, circles = rLeg, add = TRUE, bg = "red", inches = FALSE)
text(x = rep(xinit, 4) + rLeg[1] * 1.2, y = (l$y + (2 * rLeg)), rVal, cex = 0.3, srt = 0, adj = 0)
for (i in 1:4) {segments(xinit, (l$y + (2 * rLeg[i])), xinit + rLeg[1] * 1.1, (l$y + (2 * rLeg[i])))
}
text(x = xinit - rLeg[1], y = (l$y + (2 * rLeg[1])), LegTitle, adj = c(0, 0),cex = 0.7)
title(main = "population 2010",  cex.sub = 0.7)
xscale <- x2
yscale <- y1
sizescale <- 50000
labelscale <- "50km"
SpatialPolygonsRescale(layout.scale.bar(), offset = c(xscale, yscale), scale = sizescale,  fill = c("black"), plot.grid = F)
text(xscale + sizescale/2, yscale, paste(labelscale, "\n\n", sep = ""), cex = 0.7)
xarrow <- x1
yarrow <- y2 - (y2 - y1)/10
SpatialPolygonsRescale(layout.north.arrow(2), offset = c(xarrow, yarrow), scale = 50000, plot.grid = F)
i=match(fdc@data[, "id"], donnees[, "del"])
fdc@data <- data.frame(fdc@data, donnees[i,])
fdc@data$var <- fdc@data$IDRVA2011
var <- as.vector(na.omit(fdc@data$var))
nbclass <- 8
library(classInt)
distr <- classIntervals(var, nbclass, style = "quantile")$brks
library(RColorBrewer)
colours <- brewer.pal(nbclass, "YlOrRd")
colMap <- colours[(findInterval(fdc$var, distr, all.inside = TRUE))]
plot(fdc, col = colMap, border = "black", lwd = 1)
legend(x = "topright", legend = leglabs(round(distr, 2), over = "plus de", under = "moins de"),+ fill == colours, bty = "n", pt.cex = 1, cex = 0.7, title = "indice 0-1")
title(main = "Indicateur de developpement regional")


don <- donnees[donnees$reg=="TS22",]

poly <- fdc[fdc@data$id_snuts2=="TS22",]

pt <- cbind(poly@data[,"id_snuts2"], as.data.frame(coordinates(poly)))

var <- as.vector(na.omit(don$IDRVA2011))

nbclass <- 8

distr <- classIntervals(var, nbclass)$brks

colours <- brewer.pal(nbclass, "RdYlGn")
colMap <- colours[(findInterval(don$IDRVA2011, distr, all.inside = TRUE))]
plot(poly, col = colMap, border = "black", lwd = 1)
legend(x = "topright", legend = leglabs(round(distr, 2)), fill == colours, bty = "n", pt.cex = 1, cex = 0.7)
title(main = "Indicateur de developpement des regions Kairouan-Kasserine-SidiBouzid")

