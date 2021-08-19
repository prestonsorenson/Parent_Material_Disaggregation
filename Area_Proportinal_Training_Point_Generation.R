library(raster)
library(dplyr)

setwd('/home/preston/OneDrive/Papers/Parent_Material_PSM/Data')
parent_material=shapefile('sk_parent_material_10jul2021.shp')
parent_material=parent_material[parent_material$MDEP1 %in% c('COLL','EOLI','FLEO','FLLC','FLUV', 'FNPT', 'GLFL','GLLC','LACU','LATL','RESD','RKUD', 'SPPT','TILL', 'UNDM'),]

########generate random training points###########
#calculate the size of the smallest polygon in the dataset
min_area=min(sapply(parent_material@polygons, function(x) slot(x, "area")))
#generate a random number of points per polygon that scales logarithmically with the size of the polygon
randLocs=lapply(slot(parent_material, "polygons"), function(x) spsample(x,n=(ceiling(log((slot(x, 'area')/min_area)))*5), type="random"))
#randLocs=lapply(slot(parent_material, "polygons"), function(x) spsample(x, n=5, type ="random"))
#convert list of spatial points data frames to one spatial points data frame
randLocs=unlist(randLocs)
randLocs=do.call(rbind, randLocs)
crs(randLocs)=crs(parent_material)

randLocs_xy=data.frame(randLocs)
pts <- over(randLocs, parent_material)
pts=cbind(pts, randLocs_xy)

#randomly sub sample the points
#pts_sub=sample_n(pts, 100000)
pts_sub=pts


coordinates(pts_sub)=~x+y

crs(pts_sub)=crs(parent_material)

#warp to UTM NAD 83
pts_sub=spTransform(pts_sub, crs('+proj=utm +zone=13 +datum=NAD83 +units=m +no_def'))

#crop to raster extent
tri=raster('/media/preston/My Book/Saskatchewan/sk_elevation/tri_size10_guassian75_3x3.tif')
pts_sub=crop(pts_sub, tri)

shapefile(pts_sub, 'DSMART_generated_training_points_11Jul2021.shp', overwrite=TRUE)


