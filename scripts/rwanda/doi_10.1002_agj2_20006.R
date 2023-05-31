carob_script <- function(path){
  "
 Title: Standardizing local dataset from pdf tables.
 Standrdized Rice yield under different fertilization treatments from Rwanda different AEZs.
 
  "
  # path <- '~/Alliance/EiA/Agwise/From_Meklit/pdf'
  # f <- '~/Alliance/EiA/Agwise/From_Meklit/pdf/U20ArtNabahunguLowlandInthomDev.pdf'
  dataset_id <- "DOI_10.1002_agj2.20006"
  group <- "fertilizer"
  
  ## dataset level data 
  dset <- data.frame(
  dataset_id = dataset_id,
  group=group,
  uri=uri,
  publication=NA,
  data_citation = "Lowland rice yield and profit response to fertilizer application
in Rwanda. DOI: 10.1002/agj2.20006" ,
  
  data_institutions = "IITA",
  carob_contributor="Cedric Ngakou & Andrew Sila",
  experiment_type="fertilizer",
  has_weather=FALSE,
  has_management=TRUE
)

if (!require("remotes")) {
  install.packages("remotes")
}

remotes::install_github("reagro/carobiner")
remotes::install_github(c("ropensci/tabulizerjars", "ropensci/tabulizer"), INSTALL_opts = "--no-multiarch")
library(tabulizer)
library(carobiner)

out1 <- as.matrix(extract_tables(f, output = "data.frame", pages = 3, method = 'decide')[[1]])

# Drop table title
d1 <- out1[-c(1:4),]

header = d1[[1]]

header <- scan(text = header, what = "")

rows <- NULL
for (r in 2:length(d1)){
row <- scan(text = d1[[r]], what = "")
rows <- rbind(rows,row)
}

# Get location and soil details
meta <- rows[,1:6]

colnames(meta) <- header[1:6]

# Get dates for the first season and column bind
planting_date <- paste0(rows[,7],"/",gsub(".","",rows[,8], fixed = TRUE),"/",rows[,9])

harvesting_date <- paste0(rows[,10],"/",gsub(".","",rows[,11], fixed = TRUE),"/",rows[,12])

d11 <- cbind(meta,planting_date, harvesting_date)

# Get dates for the second season and bind
planting_date <- paste0(rows[,13],"/",gsub(".","",rows[,14], fixed = TRUE),"/",rows[,15])

harvesting_date <- paste0(rows[,16],"/",gsub(".","",rows[,17], fixed = TRUE),"/",rows[,18])

d12 <- cbind(meta,planting_date, harvesting_date)

table1 <- data.frame(rbind(d11,d12))

colnames(table1) <- c('location', 'cluster', 'latitude', 'longitude', 'altitude', 'soiltype', 'planting_date','harvesting_date')

# 2. Read the second table with soil data
soil0 <- extract_tables(f, output = "data.frame", pages = 4, method = 'decide')[[1]]

soil <- soil0[1:9,]

soil <- soil[-1,]

cn <- strsplit(soil$C.N, " ")

cn = data.frame(do.call(rbind,cn))

#camg <- strsplit(soil$Ca.Mg, " ")

#camg = data.frame(do.call(rbind,camg))

soil <- cbind(soil[,1:2],cn, soil[,4], soil[,c(7:9, 11,13,15)])

names(soil) <- c('location', 'pH', 'C', 'N', 'K', 'Ca', 'Mg', 'P', 'S', 'Zn', 'B')

# ---------------------------------------------------------------------------------
# Merge by location table 1 and soil
loc_soil <- merge(table1, soil, by = 'location')

# 3.  Then get table three with fertilizer rates put together with soil data when reading

fert <- soil0[-c(1:9),]

fert <- fert[-1,]

names(fert) <- fert[1,]

# remove first row with treatment codes
fert <- fert[-1,]

# Due to different lengths the values are not well aligned.

# The three rows can quickly be captured correctly:
nitrogen <- c(rep(c(0,30, 60, 90, 120),2), rep(90,5))
phosporus <- c(rep(0,5), rep(15, 5), 7.5, 22.5, rep(15,3))
potassium <- c(rep(0,12), 10,20,30)

fert <- rbind(nitrogen, phosporus, potassium)
fert <- as.data.frame(cbind(rownames(fert), fert))
names(fert) <-  c("Treatment", paste0('T',1:15))
rownames(fert) <- NULL

fert <- fert0[-1,]

# 4. Read yield table with Nrates
ylds <- extract_tables(f, output = "data.frame", pages = 6, method = 'decide')[[1]]

# Exclude the coefficients' columns and the rows 1 and 2
ylds <- ylds[-c(1:2),1:7]

colnames(ylds) <- c('Clusters', 'N0', 'N30', 'N60', 'N90', 'N120', 'MgSZnB')

# Remove the superscript in cluster B
ylds$Clusters[2] <- "B"

ylds <- ylds[-5,]

ylds_long <- cbind(ylds[,1],stack(ylds[,-1]))
names(ylds_long) <- c('Cluster','yield', 'nitrogen_rate')
ylds_long$phosporus_rate <- 'P0'
ylds_longp15 <- ylds_long
ylds_longp15$phosporus_rate <- 'P15'
# Yields for nitrogen rates and p rates averaged at p0 and p15
ylds <-  rbind(ylds_long, ylds_longp15)

#merge to loc_soil
loc_sol_ylds <- merge(loc_soil, ylds, all.x = TRUE, all.y = TRUE, by.x = 'cluster', by.y = "Cluster")

# potassium rate was at zero
loc_sol_ylds$potassium_rate <- 'K0'

# 5. Read yield table with P and K rates
ylds_p <- extract_tables(f, output = "data.frame", pages = 7, method = 'decide')

cluster <- c('A', 'B', 'BC', 'C', 'Combined')
P0 <- c(4.37, 3.88, 3.92, 3.64, 3.95)
P7.5 <- c(5.52, 5.05, 5.06, 4.59, 5.05)
P15 <- c(5.4, 4.99, 4.87, 4.58, 4.96)
P22.5 <- c(6.06, 5.48, 5.58, 4.83, 5.48)
ylds_p <- as.data.frame(cbind(cluster, P0, P7.5, P15, P22.5))

K0 <- c(4.82, 4.35, 4.31, 4.05, 4.38)
K10 <- c(5.94, 5.54, 5.50, 4.87, 5.46)
K20 <- c(6.45, 6.09, 5.94, 5.38, 5.96)
K30 <- c(6.42, 6.03, 5.99, 5.42, 5.97)

ylds_k <- as.data.frame(cbind(cluster, K0, K10, K20, K30))

ylds_p <- ylds_p[-5,]

ylds_k <- ylds_k[-5,]

yldsp_long <- cbind(ylds_p[,1],stack(ylds_p[,-1]))

yldsk_long <- cbind(ylds_k[,1],stack(ylds_k[,-1]))

names(yldsp_long) <- c('cluster','yield', 'phosporus_rate')
yldsp_long$nitrogen_rate <- 'N90'

names(yldsk_long) <- c('cluster','yield', 'potassium_rate')

yldsk_long$nitrogen_rate <- 'N90'

m <- which(yldsp_long$phosporus_rate %in% c('P0','P15'))

k <- which(yldsk_long$potassium_rate =='K0')

yldsp_long <- yldsp_long[-m,]

yldsk_long <- yldsk_long[-k,]

yldsp_long$potassium_rate <- 'K0'

yldsk_long$phosporus_rate <- 'P15'

tabs6 <-   rbind(yldsk_long, yldsp_long[,colnames(yldsk_long)])

#merge to loc_soil to tab6
loc_sol_ylds2 <- merge(loc_soil, tabs6, all.x = TRUE, all.y = TRUE, by = 'cluster')

## Rbind the two loc_sol_yld tables
out <- rbind(loc_sol_ylds, loc_sol_ylds2[,colnames(loc_sol_ylds2)])

# Add columns
out$crop<-"rice "
out$country<-"Rwanda"
out$on_farm <- TRUE
out$is_survey <- FALSE
out$irrigated <- NA
out$trial_id<- paste0(dataset_id,"-",out$soiltype)
out$start_date <- out$planting_date
out$end_date <- out$harvesting_date
out$soil_pH <- out$pH
out$soil_SOC <- out$C
out$soil_N <- out$N
out$soil_K <- out$K
out$soil_P_avilable <- out$P
out$N_fertilizer <- out$nitrogen_rate
out$K_fertilizer <- out$potassium_rate
out$P_fertilizer <- out$phosporus_rate
out <- out[,c('trial_id', 'crop', 'country', 'on_farm','is_survey', 'irrigated', 'start_date', 'end_date','soil_pH', 'soil_SOC', 'soil_N','soil_K', 'N_fertilizer', 'P_fertilizer', 'K_fertilizer', 'yield')]

carobiner::write_files(dset, out, path, dataset_id, group)  
}
