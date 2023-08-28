# Resources
## https://pjbartlein.github.io/REarthSysSci/netCDF.html#create-and-write-a-netcdf-file

# Dependencies
library(ncdf4)
library(raster)
library(tidyverse)
library(fields)
library(lubridate)
library(data.table);

# Set the working directory to the location of "ncdf_lecture"
# This directory should at least have the file "world_vi.nc" and this rscript.
# Easiest Way is to go to Session->'Set Working Directory'->'To Source File Location'
## Then on the right-> go to (cog) More -> 'Go To Working Directory'
list.files("./")

#############################
### Reading a netCDF File ###
#############################

ncname = "./world_vi.nc"
ncfile = nc_open(ncname)

# Reading variables / dimensions
names(ncfile$var) # all variables available
names(ncfile$dim) # all dimensions available

ncvar_get(ncfile,'VI') # variable data
ncvar_get(ncfile,'lat')  # dimension data
ncvar_get(ncfile,'lon')  # dimension data 
ncvar_get(ncfile,'time')  # dimension data

# Reading attributes
ncatt_get(ncfile, 0)    # list of all global
ncatt_get(ncfile, 'VI') # list of attributes for specific variable

#######################
### Extracting Data ###
#######################

# ncvar_get() - extracts variable/dimensional data
lon = ncvar_get(ncfile, "lon")   # longitude
lat = ncvar_get(ncfile, "lat")   # latitude
time = ncvar_get(ncfile, "time") # time (days since origin)
val = ncvar_get(ncfile, "VI")    # variable (Vegetation Index)

###################################################
### Plotting data with image() and image.plot() ###
###################################################
rot180 <- function(x){y1=t(x);y2=t(apply(y1,2, rev)); return(y2)}

# Important: image() and image.plot() reads y coordinate date from 
## bottom->top and left-> right, where as rasters and netCDF's are 
## indexed from top->bottom left->right, which is why we must sort()
## the latitude and longitude vectors from decreasing->increasing value,
## and transpose->rotate the array of data using rot180() defined above

# Plotting with image() function
par(mfrow=c(1,1)) # this sets plotting window grid to single plot

dtmp = val[,,1] # Extract first time slice of data from the array
ttmp = as.Date(time[1],origin = as.Date("1900-01-01"))  # Extract first date and convert from days since origin to date
image(x=lon %>% sort(),      # sort lontiude from lowest->highest
      y=lat %>% sort(),      # sort latitude from lowest->highest
      z=rot180(dtmp),        # transpose->rotate array to match ^
      main=paste("VI",ttmp), # set the plot title      ,
      xlab ="Longitude",     # set xaxis label
      ylab="Latitude",       # set yaxis label
      col = hcl.colors(12, "YlGn", rev = TRUE) # set the color pallete to green/yellow
)

# Plotting with image.plot() function to create a legend
image.plot(x=lon %>% sort(),      # sort lontiude from lowest->highest
      y=lat %>% sort(),      # sort latitude from lowest->highest
      z=rot180(dtmp),        # transpose->rotate array to match ^
      main=paste("VI",ttmp), # set the plot title      ,
      xlab ="Longitude",     # set xaxis label
      ylab="Latitude",       # set yaxis label
      col = hcl.colors(12, "YlGn", rev = TRUE) # set the color pallete to green/yellow
)


# Plotting the first 9 dates in single plotting window
n = 9 # number of items to loop through
par(mfrow=c(3,3)) # set plot window to a 3x3 grid pattern

for(i in 1:n){
  dtmp = val[,,i] # extract time slice i
  ttmp = time[i] %>% as.Date(.,origin = as.Date("1900-01-01")) # extract date i
  
  image(x=lon %>% sort(),      # sort lontiude from lowest->highest
             y=lat %>% sort(),      # sort latitude from lowest->highest
             z=rot180(dtmp),        # transpose->rotate array to match ^
             main=paste("VI",ttmp), # set the plot title      ,
             xlab ="Longitude",     # set xaxis label
             ylab="Latitude",       # set yaxis label
             col = hcl.colors(12, "YlGn", rev = TRUE) # set the color pallete to green/yellow
  )

}

# With image.plot() now to add a legend

for(i in 1:n){
  dtmp = val[,,i] # extract time slice i
  ttmp = time[i] %>% as.Date(.,origin = as.Date("1900-01-01")) # extract date i
  
  image.plot(x=lon %>% sort(),      # sort lontiude from lowest->highest
        y=lat %>% sort(),      # sort latitude from lowest->highest
        z=rot180(dtmp),        # transpose->rotate array to match ^
        main=paste("VI",ttmp), # set the plot title      ,
        xlab ="Longitude",     # set xaxis label
        ylab="Latitude",       # set yaxis label
        col = hcl.colors(12, "YlGn", rev = TRUE) # set the color pallete to green/yellow
  )
  
}

############################################
### Calculating statistics from the data ###
############################################

# Temporal average of all grid cells

# obtain average of each grid cells in time direction
val_avg = apply(val,c(1,2),mean,na.rm=T) 

# Plot the data
par(mfrow=c(1,1))
image.plot(x=lon%>%sort(), y=lat%>%sort(), z=rot180(val_avg),
           main="Average VI 2020-2022",
           col = hcl.colors(12, "YlGn", rev = TRUE),
           xlab = "Longitude",
           ylab = "Latitude")


# Average for each year

# Step1: Get year of each time slice (years will repeat)
time  # time is in says since origin, so convert->date->extract year
time_dates = time %>% as.Date(., origin=as.Date("1900-01-01"))
time_years = time_dates %>% lubridate::year(.)

# Step2: Find all time dimension index positions associated with each year
dt = data.table::as.data.table(time_years)[, list(list(.I)), by = time_years]
names(dt) = c("year", "idx") # rename columns

# Step3: Loop through all years, subset the array using these indexes
# Step4: During the loop: get the average of the new subsetted array
# Step5: During the loop: save the subsetted array a working list

val_year = lapply(dt$idx, function(Indexes){
  apply(val[,,Indexes],c(1,2), mean, na.rm=T)
})

class(val_year)       # class of object
length(val_year)      # length of list (3 years)
lapply(val_year,dim)  # dimension of each sublist array
dim(val)              # dimension of original array

# Step6: Convert the list of subsetted matixes to an array
val_year = simplify2array(val_year)

class(val_year)
dim(val_year)

# Step7: Loop through all years and plot

n = length(dt$year)
par(mfrow=c(2,2))

for(i in 1:n){
  # pull out single time slice (year)
  dat = val_year[,,i] 
  
  # Plot the time slice
  image.plot(x=lon %>% sort(), y=lat %>% sort(), z=rot180(dat),
             #main=paste("NDVI",time[i]),
             main=paste("Average VI", dt$year[i]),
             col = hcl.colors(12, "YlGn", rev = TRUE),
             xlab ="Longitude",
             ylab="Latitude"
  )
}


# Percentage Change between each year (2020 -> 2021 and 2021 -> 2022)

# Step1: Create empty list to store %-change of each time slice
## and a vector identifying the two years used for calculation
val_year_pchg = list()
taglist = vector()

# Step2: Loop through each time slice (year) starting with index 2
n = dim(val_year)[3]
par(mfrow=c(1,2))

for(i in 2:n){
  
  # Step3: Get the percent difference and the tag of the current time slice
  ## and the previous time slice
  dtmp = (val_year[,,(i-1)] - val_year[,,i])/val_year[,,(i-1)]
  ttmp = paste("%-Chg ","(",dt$year[(i-1)],"-",dt$year[i],")",sep="")
  
  # Step4: Save to the working list and vector() for storage
  val_year_pchg = append(val_year_pchg, list(dtmp))
  taglist = append(taglist,ttmp)
  
  # Step5: Plot the %-chg for these years
  image.plot(x=lon %>% sort(), y=lat %>% sort(), z=rot180(dtmp),
             main=ttmp,
             col = hcl.colors(12, "YlGn", rev = TRUE),
             xlab ="Longitude",
             ylab="Latitude"
  )
}

# Step6: Convert the list of matrices to an array for storage
val_year_pchg = simplify2array(val_year_pchg)


#######################################################
### Subsetting the netcdf data to a smaller extent  ###
###              (Global to CONUS)                 ####
#######################################################

# Step1: Select your extent of interest (we use CONUS Boundary)
ext_usa = c(23.81, -129.17, 49.38, -65.69) # c(South-West, North-East)

# Step2: Filter your longitude and latitude
## Create a vector of array index values (one for x-dim and y-dim)
## which fall within your new spatial boundary

# Step2a: Create bool vector containing index positions in new bounds
ltest = (lon >= ext_usa[2] & lon <= ext_usa[4]) # index values
dim(ltest)
head(ltest)

# Step2b: Use bool vector to create index vector
lon_conus_idx = (1:length(lon))[ltest]
length(lon_conus_idx)
lon_conus_idx

# Step3c: Use index vector to create subsetted dimension vector
lon_conus = lon[lon_conus_idx]
dim(lon_conus)
lon_conus %>% range() # bounds of latitude
lon_conus

# Step4d: Now do the same for latitude
ltest = (lat >= ext_usa[1] & lat <= ext_usa[3])
lat_conus_idx = (1:length(lat))[ltest]
lat_conus = lat[lat_conus_idx]
lat_conus %>% range() # range of latitude

# Step5: Subset the array of data using the index vectors
val_conus = val[lon_conus_idx, lat_conus_idx, ]
class(val_conus)
dim(val_conus) # long, lat, time

# Step6: Plot to check your work
## Look at first date
par(mfrow=c(1,1))
image.plot(lon_conus %>% sort(), lat_conus %>% sort(), rot180(val_conus[,,1]),
      main=paste("CONUS Vegetation Index", time_dates[1]),
      col = hcl.colors(12, "YlGn", rev = TRUE),
      xlab = c("Longitude"), ylab = c("Latitude")
      )

# Step7: Plot first 9 dates
n=9
par(mfrow=c(3,3))
for(i in 1:n){
  image.plot(lon_conus %>% sort(), lat_conus %>% sort(), rot180(val_conus[,,i]),
             main=paste("CONUS Vegetation Index", time_dates[i]),
             col = hcl.colors(12, "YlGn", rev = TRUE),
             xlab = c("Longitude"), ylab = c("Latitude")
  )
}


# Note: Ignore NA areas for now, they can be fixed with raster()
## functions

###########################################
### Create netCDF with this new dataset ###
###########################################
# Goal: Keep similar attributes as before
ncfile 

# Tip: Save the proj4 format CRS string from original netCDF
crs_proj4 = ncatt_get(ncfile,0,'crs')$value
crs_proj4

# Close the currently open netcdf file
nc_close(ncfile)

# Step1: Create dimensions of new netCDF
## create a dimension for x (lon), y (lat), and z (time)
## Define dimensions with ncdim_def() function of ncdf4::

# Step1a: longitude
dim_lon = ncdim_def(name="lon", units="degrees_east", 
                    vals=lon_conus, longname="latitude")

dim_lon # Creates an object compatable for netCDF creation function

# Step1b: latitude
dim_lat = ncdim_def("lat","degrees_north",
                    lat_conus, longname="longitude")

# Step1c: time
time_conus = time # we are using all the same dates
dim_time = ncdim_def("time","days_since_origin", 
                     time_conus, 
                     longname="days since 1900-01-01")

# Step2: Create netCDF Variable(s)
## In our case we only have 1 variable, Vegetation Index (VI)
## Define variables of netCDF with ncvar_def()
## This function uses a list of dimensions created in previous step
## Standard order of dimensions is (x (lon), y (lat), z (time))

novalue = NA # value associated with no data
lgname = "Vegetation Index (eVIIRS Satellite)"
var_vi = ncvar_def(name="VI", units="Unitless", 
                   dim=list(dim_lon, dim_lat, dim_time), 
                   missval=novalue, 
                   longname=lgname, 
                   prec="double")

var_vi # similar style object as dimension objects

# Step3: Create an "empty" netCDF using the variables created.
## "empty" means there gridded data input yet, it is only a 
## skeleton of the netCDF, with variables/attributes, and 
## associated dimensions/attributes defined
## Create empty netCDF file with nc_create()

fname = "vi_conus2.nc" # name of the nc file we will create

# If your get an Error, change the name above. Its because
## the program is trying to write over an existing file
## with the same name; optionally you can delete the original
## I sent out.
ncfile = nc_create(filename=fname, 
                   vars=list(var_vi), 
                   force_v4=TRUE)

ncfile # check out your new netCDF file (skeleton)

# Step3b: Check to see that the new netCDF is empty.
## It should have created an array with the correct
## dimensions, but all values are Nan
val = ncvar_get(ncfile, "VI")
dim(val) # correct dimensions
sum(is.na(val[])) == length(val[]) # all values are Nan

# Step4: Insert the gridded data associated with all variables
## Use function ncvar_put() to do this
ncvar_put(nc=ncfile, 
          varid=var_vi,    # variable object we created before
          vals=val_conus   # array of values we created for CONUS
          )

# Step5: Add additional attributes to the netCDF
## We use ncatt_put() function to do this

# Step5a: Dimensional attribues
ncatt_put(nc=ncfile, 
          varid="lon",    # name of dimension
          attname="axis", # name of new attribute
          attval="X")     # value of new attribute

ncatt_put(ncfile,"lat","axis","Y")
ncatt_put(ncfile,"time","axis","T")

# Step5b: Variable attributes
ncatt_put(ncfile, 'VI', 'Note', 'Just added an attribute')

# Step5c: Global attributes
## these are not linked to a certain variable or dimension
ncatt_put(nc=ncfile,       # ncfile
          varid=0,         # '0' marks global attribute
          attname="crs",   # name of new attribute ('crs')
          attval=crs_proj4 # string of crs we saved earlier
          )

ncfile

ncatt_put(ncfile,0,
          "title","Test NCDF File Creation")     # title
ncatt_put(ncfile,0,
          "institution","WSU R Working Group")   # institution
ncatt_put(ncfile,0,
          "source","WSU R Working Group")        # source
ncatt_put(ncfile,0,
          "references","None")                   # references
history <- paste("Jonathan Gendron", date(), sep=", ")    # history
ncatt_put(ncfile,0,
          "history",history)                     # history
ncatt_put(ncfile,0,"Conventions","None")         # conventions

ncfile

# Step6: Test data extraction from your new netCDF file
val2 = ncvar_get(ncfile, 'VI')
dim(val2)

lon2 = ncvar_get(ncfile, 'lon')
dim(lon2)

lat2 = ncvar_get(ncfile, 'lat')
dim(lat2)

time2 = ncvar_get(ncfile, 'time')
dim(time2)

# Step7: Plot your data to test if it was created correctly
par(mfrow=c(1,1))
image.plot(lon2 %>% sort(), lat2 %>% sort(), rot180(val2[,,1]),
           main=paste("CONUS Vegetation Index", time_dates[1]),
           col = hcl.colors(12, "YlGn", rev = TRUE),
           xlab = c("Longitude"), ylab = c("Latitude")
)

# Success!

########################################################
### Extract only certain extent directly from ncfile ###
########################################################

# Goal: Extract data for an extent approximately around
## the state of florida ~(23,-85, 35,-75) [(SW, NE) boundaries]

# Step1: Create index vectors for x (lon) and y (lat)
## just like before.
lon3_idx = (1:length(lon2))[(lon2 >= -85 & lon2 <= -75)]
lat3_idx = (1:length(lat2))[(lat2 >= 23 & lat2 <= 35)]

# Step2: Define the starting point index position for each
## dimension to be used in the extraction function ncvar_get()
## Note: starting position should be the minimum of your
### index vectors of each dimension
lon3_start = min(lon3_idx) 
lat3_start = min(lat3_idx) 

# Step3: Define the index length to be used for each
## dimension in the extraction function ncvar_get().
## Note: length should equal length of index vector
### and it measure the number of indexes to count up
### from (left->right for x or bottom->top for y)
### when data is extracted from the variable array.
lon3_count = length(lon3_idx) 
lat3_count = length(lat3_idx) 


# Step 4: Extract data using the ncvar_get() function
## whilst also defining the start position of each dimension
## and the length of each dimension to extract (positivly)

val3 = ncvar_get(ncfile,  # open ncfile 
                 'VI',    # variable name
                 
                 start=c(lon3_start, # xdim start
                         lat3_start, # ydim start
                         1), # zdim start (we want 1)
                 
                 count=c(lon3_count, # xdim count
                         lat3_count, # ydim count
                         -1) # zdim count (-1 denotes full length)
)

dim(val3) # dimensions of the new array
paste(length(lon3_idx), # intended xdim length 
      length(lat3_idx), # intended ydim length
      length(time2)     # intended zdim length
      )    

# Step5: Plot your extracted data!

# Step5a: Create your dimensional vectors by
## using your index vectors
lon3 = lon2[lon3_idx]
lat3 = lat2[lat3_idx]

# Step5b: Plot the first date
par(mfrow=c(1,1))
image.plot(lon3 %>% sort(), lat3 %>% sort(), rot180(val3[,,1]),
           main=paste("Florida Vegetation Index", time_dates[1]),
           col = hcl.colors(12, "YlGn", rev = TRUE),
           xlab = c("Longitude"), ylab = c("Latitude")
)


nc_close(ncfile) # close the open nc_file

###########################################################
### Opening netCDF files directly with raster() package ###
###########################################################

# Loading the entire netcdf as a raster stack

# Step1: Set filename
fname="vi_conus.nc"

# Step2: create raster stack using raster::stack()
## This is a multiband raster, wherein each band
## is another time slice.
rstack = raster::stack(fname)
rstack      # attriutes for the raster stack
dim(rstack) # same dimensions at the netCDF

# Step3: Correct the crs if neccessary
## Sometimes this method will use a default CRS
## that is not defined by the netCDF file

# Step3a: Compare crs of raster stack to crs of netCDF
crs_rstack = crs(rstack)@projargs
crs_ncdf = crs_proj4
crs_rstack == crs_proj4 # it has one extra attribute +no_defs

# Step3b (optional): Set the crs of the raste stack
crs(rstack) = crs_ncdf

# Step4: Extract a subset of time slices from raster stack
## each time slice corresponds to a raster band

# Step4a: Create list of rasters you want to subset
rlist = rstack@layers[1:9] # first 9 bands

# Step4b: Create a raster stack from this list
rstack2 = stack(rlist)

# Step4c (Optional) Change the name of each raster band
## to the dates of each time slice
names(rstack2) = time_dates[1:9] # rename each band to date

# Step5: Plot a single band
par(mfrow=c(1,1))
r1 = rstack2@layers[[1]] # extract the first band
d1 = names(r1)
plot(r1,
     col = hcl.colors(12, "YlGn", rev = TRUE),
     main = paste("Vegetation Index for CONUS", d1),
     xlab = "Longitude", ylab ="Latitude"
     ) # plot

# Step6: Plot first 9 dates
par(mfrow=c(3,3))
plot(rstack2,
     col = hcl.colors(12, "YlGn", rev = TRUE)
     )

# # Step7 (Optional) advantage: NAfill with raster package's focal()
# r2 = focal(rstack2@layers[[1]], w=matrix(1/9,3,3), fun=mean, NAonbly=TRUE, na.rm=TRUE)
# plot(r2, col = hcl.colors(12, "YlGn", rev = TRUE))

######################################
### Write netcdf from raster stack ###
######################################
# Goal: Create a raster stack from scratch,
## then write it to file in netCDF format

# Step1: Create a raster stack of length 3, wherein each raster
## is 7 rows x 6 cols x 3 time slices, and each time slice depicts
## the numbers 1, 2, 3 respectively. Values only need to be binary.

# Step1a: Create the number 1 raster
par(mfrow=c(1,3))
r1 = matrix(c(
  c(0,0,1,1,0,0),
  c(0,0,0,1,0,0),
  c(0,0,0,1,0,0),
  c(0,0,0,1,0,0),
  c(0,0,0,1,0,0),
  c(0,0,0,1,0,0),
  c(0,1,1,1,1,1)
), nrow=7, ncol=6, byrow=TRUE
) %>% raster()
plot(r1) # check the raster

# Step1b: Create the number 2 raster
r2 = matrix(c(
  c(0,0,1,1,0,0),
  c(0,1,0,0,1,0),
  c(0,0,0,0,1,0),
  c(0,0,0,1,0,0),
  c(0,0,1,0,0,0),
  c(0,1,0,0,0,0),
  c(1,1,1,1,1,0)
), nrow=7, ncol=6, byrow=TRUE
) %>% raster()
plot(r2) # check the raster

# Step1c: Create the number 3 raster
r3 = matrix(c(
  c(0,0,1,1,0,0),
  c(0,1,0,0,1,0),
  c(0,0,0,0,1,0),
  c(0,0,1,1,0,0),
  c(0,0,0,0,1,0),
  c(0,1,0,0,1,0),
  c(0,0,1,1,0,0)
), nrow=7, ncol=6, byrow=TRUE
) %>% raster()
plot(r3)

# Step2: Create a raster stack of the three rasters
rstack2 = stack(r1,r2,r3)
names(rstack2) = c("1","2","3") # change names if desired

# Step3: Save a netCDF from the raster stack
fname = "number.nc"
writeRaster(rstack2,           # raster stack to be saved
            fname,             # name of netcdf file to write
            format="CDF",      # set the output format to CDF (netCDF)
            varname="number",  # set the name of the variable that will be written
            varunit="integer", # set the unit of the variable that will be written
            xname="x",         # set the name of the x-dimension
            yname="y"          # set the name of the y-dimension
            )

# Step4: Read the netCDF file that you just created
ncfile = nc_open(fname)

# Step5: Extract variables from the netCDF
num = ncvar_get(ncfile, 'number')
vx = ncvar_get(ncfile, "x")
vy = ncvar_get(ncfile, "y")

# Step6: Plot the data to check if written correctly
par(mfrow=c(1,3))

for(i in 1:dim(num)[3]){
  image(vx %>% sort(), vy %>% sort(), rot180(num[,,i]), xlab=NULL, ylab=NULL)
}

# Success!

nc_close(ncfile) # Close the open netCDF file

# Thanks for attenting this lession!

