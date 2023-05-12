
path
carob_script <- function(path){
  
  "
 Title: Test of using local dataset, has no uri.
 Aggregated Data sample for Rice yield under different fertilization treatments, in African countries.
 
  "
  
  # uri <- "doi:10.25502/6G5B-RM44/D"
  dataset_id <- "doi_RICE_001"
  group <- "fertilizer"
  
  ## dataset level data
  
  dset <- data.frame(
    dataset_id = dataset_id,
    group=group,
    project=NA,
    publication= NA,
    data_citation = "CGIAR Aggregated Data sample for Rice yield",
    data_institutions = "CGIAR",
    carob_contributor="Rachel Mukami and Samar Attaher ",
    experiment_type="Fertilizer trial",
    has_weather=FALSE,
    has_management=FALSE
  )
  
  ## download and read data
  # uri, 
  # ff <- carobiner::get_data(path, group)
  # js <- carobiner::get_metadata(dataset_id, path, group, major=1, minor=0)
  # dset$license <- carobiner::get_license(js)
  
  
  ###download and read data
  
  # f <- ff[basename(ff) == "data.csv"]
  #d <- data.frame(read.csv("C:/Users/User/Documents/IITA/carob/data/raw/fertilizer/ICARDA RICE/Aggregated_Rice_Africa.csv"))
  d <- data.frame(read.csv("carob/Data/raw/Aggregated_Rice_Africa.csv"))
  d <- d[!is.na(d$Yield),] # dropping any entry without yield input
  d$country <- carobiner::fix_name(d$Country, "title")
  d <- d[!is.na(d$country),]
  d$country <- carobiner::replace_values(d$country,
                                         c("Thegambia","Burkinafaso","Burkina","Sierraleone","Rci"),
                                         c("Gambia","Burkina Faso","Burkina Faso","Sierra Leone","Cote d'Ivoire"))
  d$site <- carobiner::fix_name(d$SiteName,"title")
  d$site[d$site == "Savelugu"] <- "Savelugu Nanton"
  d$Village <- iconv(d$Village, to = "UTF-8", sub = "")
  d$location <- carobiner::fix_name(d$Village,"title")
  d$latitude <- as.numeric(d$Latitude)
  d$longitude <- as.numeric(d$Longitude)
  d$latitude[d$country == "Senegal"] <- 14.4750607
  d$longitude[d$country == "Senegal"] <- -14.4529612
  d$latitude[d$country == "Mali"] <- 16.3700359
  d$longitude[d$country == "Mali"] <- -2.2900239
  d$latitude[d$country == "Burkina Faso"] <- 12.0753083
  d$longitude[d$country == "Mali"] <- -1.6880314
  d$latitude[d$site == "Keur Macene"] <- 16.5549676
  d$longitude[d$site == "Keur Macene"] <- -16.2353267 
  d$latitude[d$site == "Gouer"] <- 16.533333 
  d$longitude[d$site == "Gouer"] <- -15.966667 
  d$latitude[d$site == "Garack"] <- 16.55593025 
  d$longitude[d$site == "Garack"] <- -15.519789048129034
  d$latitude[d$site == "Leixeiba"] <- 16.2168413 
  d$longitude[d$site == "Leixeiba"] <- -13.1449596
  d$latitude[d$site == "Foumgleita"] <- 16.0905988
  d$longitude[d$site == "Foumgleita"] <- -12.740206686799427 
  d$latitude <- ifelse(is.na(d$latitude), 20.2540382,d$latitude)
  d$longitude <- ifelse(is.na(d$longitude),-9.2399263,d$longitude)
  d$soil_type <- carobiner::fix_name(d$SoilType,"title")
  d$soil_type <- carobiner::replace_values(d$soil_type,
                                           c("Gleysol","Vert/gleysol","Ac"),
                                           c("Gleysols","Vertisols,Gleysols","Acrisols"))
  d$dataset_id <- dataset_id
  d$trial_id <- paste(d$country, d$ID, sep = "_")
  d$rain <- as.numeric(d$TotalRainfall) #assumption is it's in mm
  d$tillage <- carobiner::fix_name(d$Tillage)
  d$irrigated <- ifelse(grepl("irrigated",d$ProductionSystem),TRUE,FALSE)
  d$season <- d$Season
  d$crop <- "rice"
  d$Variety <- iconv(d$Variety, to = "UTF-8", sub = "")
  d$variety <- carobiner::fix_name(d$Variety,"title")
  d$N_fertilizer <- d$Nrate # assumption is it's in kg/ha
  d$P_fertilizer <- d$Prate
  d$K_fertilizer <- d$Krate
  d$end_date <- as.character(as.Date(d$HarvestingDate)) 
  d$start_date <- as.character(as.Date(d$DateSeeding))
  d$treatment <- carobiner::fix_name(d$Treatment)
  
  d$yield <- d$Yield*1000 #converting kg/ha
  
  gg <- d[,c("dataset_id","trial_id","country","site","location","latitude","longitude","soil_type","rain","tillage","irrigated",
             "season","crop","variety","N_fertilizer","P_fertilizer","K_fertilizer","start_date","end_date","treatment","yield")]

  
  #carobiner::write_files(dset, gg, path, dataset_id, group)
  carobiner::write_files(dset, gg,path, dataset_id, group)  #(Samar): I removed the [path] becouse it returns error
  install.packages("writexl")
  library(writexl)
  write_xlsx(gg,"carob/Data/Final/doi_na_aggregated_rice.xlsx")
  write.csv(gg, 'carob/Data/Final/doi_na_aggregated_rice.csv')
}
