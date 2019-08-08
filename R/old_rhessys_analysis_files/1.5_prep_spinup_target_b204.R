# Code for creating spinup targets


# ---------------------------------------------------------------------
# Generate target file for b204

# read in, add basin, zone, stratum
hill_ID=scan(file="ws_b204/auxdata/hill.asc", skip=6, na.strings="*")
patch_ID=scan(file="ws_b204/auxdata/patch.asc", skip=6, na.strings="*")
zone_ID = patch_ID
stratum_ID = rep(1,length(hill_ID))
LAI=scan(file="ws_b204/auxdata/lai.asc", skip=6, na.strings="*")
#LAI=round(LAI, digits=5)

x=rep(1,length(hill_ID)) # uses the number of cases
basin_ID=x

#combine them
tmp = as.data.frame(cbind(basin_ID,hill_ID,zone_ID,patch_ID,stratum_ID,LAI))
tmp2 = subset(tmp, is.na(tmp$patch_ID)==F)
tmp = aggregate(tmp2, by=list(tmp2$patch_ID, tmp2$zone_ID, tmp2$hill_ID, tmp2$basin_ID), mean)
#tmp = aggregate(tmp2, by=list(tmp2$basin_ID, tmp2$hill_ID, tmp2$zone_ID,tmp2$patch_ID), mean)

# Code for 2nd canopy
tmp2 = tmp
tmp2$stratum_ID = 2
tmp = rbind(tmp,tmp2)

#reorder them, remove subset_key
tmp=tmp[,c(5:10)]

# remove null values
tmp = subset(tmp, is.na(tmp$LAI)==F)
tmp = subset(tmp, !tmp$LAI==-9999) # if user has another null value, input it here
tmp = subset(tmp, !tmp$LAI==Inf) # if user has another null value, input it here
tmp = format(tmp, scientific=FALSE)

#Export and merge with a file header and target list
newheader = sprintf("%d num_stratum\n%d num_targets", nrow(tmp), length(tmp)-5)
write(newheader, file="ws_b204/tecfiles/spinup_thresholds.txt")
targets = colnames(tmp)
targets = subset(targets, !targets=="basin_ID") 
targets = subset(targets, !targets=="hill_ID") 
targets = subset(targets, !targets=="zone_ID") 
targets = subset(targets, !targets=="patch_ID")
targets = subset(targets, !targets=="stratum_ID") 
write(targets, file="ws_b204/tecfiles/spinup_thresholds.txt", append=T)
write.table(tmp, file="ws_b204/tecfiles/spinup_thresholds.txt", append=T, quote=F, row.names=F)


# ---------------------------------------------------------------------
# Add spinup_default_ID at stratum-level for doing target based spinup

world_name_in <- "ws_b204/worldfiles/b204.world"
world_name_out <- "ws_b204/worldfiles/b204_spinup_pre.world"

newrow <- tibble(a=1, b="spinup_default_ID")

# Read in worldfile
worldfile <- read_table2(world_name_in, col_names = FALSE)

aa=2
while(aa < nrow(worldfile)){
  if (aa%%1000 == 0 ){print(paste(aa,"out of", nrow(worldfile)))} # Counter
  if (worldfile[aa,2] == "cover_fraction" && worldfile[aa-1,2] == "default_ID"){
    
    # Add new line containing spinup_default_ID
    worldfile[seq(aa+1,nrow(worldfile)+1),] <- worldfile[seq(aa,nrow(worldfile)),]
    worldfile[aa,] <- newrow[1,]
  }
  aa <- aa + 1
}

# Write new file
worldfile$X1 <- format(worldfile$X1, scientific = FALSE)
write.table(worldfile, file = world_name_out, row.names = FALSE, col.names = FALSE, quote=FALSE, sep="  ")

