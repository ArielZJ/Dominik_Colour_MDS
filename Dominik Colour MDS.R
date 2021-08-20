# library needed for 3D visualisation
library(rgl)

# loading the data
data.means <- read.csv("Data/means.csv")
data.medians <- read.csv("Data/medians.csv")
data.modes <- read.csv("Data/modes.csv")

# convert the RGB values to hexcodes for visualisation
rgb2hex <- function(triplet){
  return(sapply(strsplit(triplet, ","), function(triplet)
    rgb(triplet[1], triplet[2], triplet[3], maxColorValue=255)))
}

colors <- rgb2hex(as.character(data.means$Row.RGB))
data.means$Row.RGB <- colors # start by looking at mean responses

# label the columns appropriately
col.RGB <- rev(data.means$Row.RGB)
colnames(data.means) <- col.RGB
rownames(data.means) <- data.means$Row.RGB
data.means <- subset(data.means, select = -c(94))

# convert dataframe to distance matrix for MDS
data.dist <- dist(data.means,diag=TRUE)


# peform metric MDS, set dimensionality to 3
#Calculate MDS
data.mds <- cmdscale(data.dist, k=3)

#Create x,y refs
data.x <- data.mds[,1]
data.y <- data.mds[,2]
data.z <- data.mds[,3]

# visualise the data
plot <- plot3d(data.x,data.y,data.z, col = colors, size = 10)

# 3D animated version
#play3d(spin3d(axis=c(1,1,0), rpm=3), duration=30)

# save a screenshot
rgl.snapshot('3dplot.png', fmt = 'png')