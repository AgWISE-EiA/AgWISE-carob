# R script for "carob"

## ISSUES
# this seems to be an incomplete design; 
# there is a "maize-Gliricidia/50% fertilizer" treatment, 
# but there is no "50% fertilizer" treatment; only 0 or 100%. 


carob_script <- function(path) {

"
Description:

Sileshi, G.W., Debusho, L.K. and Akinnifesi, F.K. (2012), Can Integration of Legume Trees Increase Yield Stability in Rainfed Maize Cropping Systems in Southern Africa?. Agronomy Journal, 104: 1392-1398. https://doi.org/10.2134/agronj2012.0063

Growing maize (Zea mays) in association with legume tree in agroforestry arrangements has been shown to increase yields in many parts of sub-Saharan Africa (SSA). However, the stability of crop yields has not been critically analyzed in the various cropping systems that integrate leguminous trees. The objective of this analysis was to compare yield stability in improved cropping systems, namely maize-Gliricidia (Gliricidia sepium) intercropping and fertilized monoculture maize, with the de facto practice of resource-poor farmers who grow maize continuously without any external input. Yield stability was determined for three long-term field trials (12-13 consecutive years) conducted at Makoka Research Station in southern Malawi and Msekera Research Station in eastern Zambia. At Makoka, the most stable yield was recorded in maize-Gliricidia intercrops. Average yield was highest in maize-Gliricidia intercropping amended with 50% of the recommended N and P fertilizer, and this was comparable with yield recorded in monoculture maize that received inorganic fertilizer. On the two sites at Msekera, the highest yield was recorded in fertilized monoculture maize followed by maize-Gliricidia intercrops. However, yields were more stable in maize-Gliricidia intercropping compared to fertilized maize on both sites at Msekera. It is concluded that maize yields remain more stable in maize-Gliricidia intercropping than in fertilized maize monoculture in the longterm although average yields may be higher with full fertilization.
"

	uri <- "doi:10.34725/DVN/25746"
	dataset_id <- carobiner::simple_uri(uri)
	group <- "fertilizer"
	## dataset level data 
	dset <- data.frame(
		dataset_id = dataset_id,
		group = group,
		uri = uri,
		publication = "doi:10.2134/agronj2012.0063",
		carob_contributor = "Camila Bonilla",
		experiment_type = "fertilizer;agroforestry",
		has_weather = FALSE,
		has_management = FALSE
	)

## download and read data 

	ff  <- carobiner::get_data(uri, path, group)
	js <- carobiner::get_metadata(dataset_id, path, major=1, minor=0, group)
	dset$license <- carobiner::get_license(js)

	f <- ff[basename(ff) == "Sileshi Stab analysis data.xlsx"]
	# process file(s)

	d <- carobiner::read.excel(f)
	
	d <- as.data.frame(d)
	colnames(d) <- gsub("treat$", "treatment", tolower(d[1,]))
	d <- d[-1,]
	m <- data.frame(location="Makoka", exp=1, d[, 1:4])

	m$soil_type <- 'Ferric lixisol'
	m$soil_sand <- 46
	m$soil_clay <- 46
	m$soil_SOC <- 8.8 
	m$soil_pH <- 5.9
	m$longitude <- 35.15
	m$latitude <- -15.30
	m$location <- 'Makoka'
	m$country <- 'Malawi'
	m$adm1 <- 'Machinga'
	m$K_fertilizer <- m$P_fertilizer <- m$N_fertilizer <- 0
	i <- m$treatment == "Fertilizer"
	m$N_fertilizer[i] <- 92
	m$P_fertilizer[i] <- 40
	i <- m$treatment == "Gliricidia+50%F"
	m$N_fertilizer[i] <- 46
	m$P_fertilizer[i] <- 20


	E1 <- data.frame(location="Msekera", exp=2, d[, 5:8])
	E2 <- data.frame(location="Msekera", exp=3, d[,10:13])
	e <- rbind(E1, E2)
	e$longitude <- 32.34
	e$latitude <- -13.39
	e$location <- 'Msekera'
	e$country <- 'Zambia'
	e$adm1 <- NA
	e$soil_type <- 'Ferric luvisol'
	e$soil_sand <- 61
	e$soil_clay <- 28
	e$soil_SOC <- 10.2
	e$soil_pH <- 5.3
	e$soil_P_total <- 2.02 # mg/kg
	e$soil_N <- 0.70 # g/kg
	e$soil_K <- 1.47 # cmolc kg−1

	e$K_fertilizer <- e$P_fertilizer <- e$N_fertilizer <- 0
	i <- e$treatment == "Fertilizer"
	e$N_fertilizer[i] <- 20 + 92
	e$P_fertilizer[i] <- 18
	e$K_fertilizer[i] <- 16

	m$soil_K <- NA
	m$soil_N <- NA
	m$soil_P_total <- NA

	d <- rbind(m, e)

	d$fertilizer_type <- "TSP; CAN"

	d$yield <- round(as.numeric(d$yield) * 1000)
	d$crop <- "maize"
	d$variety_type <- "hybrid"

	d$start_date <- paste0(d$year, "-11")
	year <- as.numeric(d$year) + 1
	d$end_date <- paste0(year, "-04")
	d$start_date[is.na(d$year)] <- NA
	d$end_date[is.na(d$year)] <- NA
	d$year <- NULL
	
	gli <- grep("Gliricidia", d$treatment)
	d$treatment <- "no Gliricidia"
	d$treatment[gli] <- "Gliricidia"
	d$trial_id <- paste(d$treatment, "-", d$exp)
	d$exp <- NULL
	d$on_farm <- FALSE


	d$dataset_id <- dataset_id
	d$rep <- as.integer(d$rep)
	
	carobiner::write_files(dset, d, path, dataset_id, group)
}

