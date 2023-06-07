# R script for "carob"

## ISSUES
# ....


carob_script <- function(path) {

"
	Description:

    TBD

"

	uri <- "doi:Wheat-W.-Amhara-2018"
	dataset_id <- carobiner::simple_uri(uri)
	group <- "biofortification"
	## dataset level data 
	dset <- data.frame(
	   dataset_id = dataset_id,
	   group=group,
	   project=NA,
	   uri=uri,
	   ## if there is a paper, include the paper's doi here
	   ## also add a RIS file in references folder (with matching doi)
	   publication= "",
	   data_institutions = "Excellence in Agronomy",
	   carob_contributor="Eduardo Garcia Bendito",
	   
	   ## something like randomized control...
	   experiment_type="___",
	   has_weather=FALSE,
	   has_soil=FALSE,
	   has_management=FALSE
	)

## download and read data 

	# ff  <- carobiner::get_data(uri, path, group)
	# js <- carobiner::get_metadata(dataset_id, path, group, major=2, minor=1)
	# dset$license <- carobiner::get_license(js)


	f <- "data/raw/biofortification/Agronomic Biofortification data sets.xlsx"

	r <- readxl::read_excel(f, sheet = "Wheat W. Amhara 2018") |> as.data.frame()

	
## process file(s)

## use a subset
	# d <- carobiner::change_names(r, from, to)

	
#### about the data #####
## (TRUE/FALSE)

	r$dataset_id <- dataset_id
# 	d$on_farm <- 
# 	d$is_survey <- 
# 	d$irrigated <- 
# ## the treatment code	
# 	d$treatment <- 


##### Location #####
## make sure that the names are normalized (proper capitalization, spelling, no additional white space).
## you can use carobiner::fix_name()
	d <- as.data.frame(cbind(r$dataset_id, r$Country))
	# d$country <- "Ethiopia"
	d$site <- r$Site
	d$adm1 <- r$`Region/state`
	d$adm2 <- r$`Specific Region/State`
	d$adm3 <- r$District
	d$elevation <- r$`Altitude(m)`
## each site must have corresponding longitude and latitude
	d$longitude <- r$longitude
	d$latitude <- r$latitude



##### Crop #####
## normalize variety names
	d$crop <- r$Crop
	d$variety <- r$Variety

##### Time #####
## time can be year (four characters), year-month (7 characters) or date (10 characters).
## use 	as.character(as.Date()) for dates to assure the correct format.
	d$start_date <- as.character(as.Date(r$`Planting date`))
	d$end_date  <- as.character(as.Date(r$`Harvest date`))

##### Fertilizers #####
## note that we use P and K, not P2O5 and K2O
## P <- P2O5 / 2.29
## K <- K2O / 1.2051
   d$P_fertilizer <- r$P2O5_amount
   d$K_fertilizer <- r$K2O_amount
   d$N_fertilizer <- r$N_amount...24
## normalize names 
   d$fertlizer_type <- 
   d$inoculated <- FALSE
   

##### in general, add comments to your script if computations are
##### based in information gleaned from metadata, publication, 
##### or not immediately obvious for other reasons

##### Yield #####

	d$yield <- r$`Yield (kg/ha)`
  d$residue_yield <- r$`Straw yield (kg/ha`
	d$biomass_total <- r$`Biomass (kg/ha)`
	d$grain_weight <- r$`1000 seed weight at 12.5% moisture content.`
	
##### Biofortification #####
	d$plant_P <- r$P
	d$plant_K <- r$K
	d$plant_S <- r$S
	d$plant_Mg <- r$Mg
	
# all scripts must end like this
	carobiner::write_files(dset, d, path, dataset_id, group)
}

## now test your function in a clean environment 
# path <- _____
carob_script(path)

