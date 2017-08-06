if ( .Platform$OS.type == 'windows' ) memory.limit( 256000 )

library(lodown)
# examine all available PEW microdata files
pew_cat <-
	get_catalog( "pew" ,
		output_dir = file.path( getwd() ) )

# spring 2015 only
pew_cat <- subset( pew_cat , name == "Spring 2015 Survey Data" )
# download the microdata to your local computer
stopifnot( nrow( pew_cat ) > 0 )

options( survey.lonely.psu = "adjust" )

library(survey)

pew_df <- 
	readRDS( 
		file.path( getwd() , 
		"Global Attitudes & Trends/2015/Spring 2015 Survey Data" ,
		"Pew Research Global Attitudes Spring 2015 Dataset for Web FINAL.rds" )
	)

# limit the global attitudes data set to just israel
israel_df <- subset( pew_df , country == 14 )
	
pew_design <- 
	svydesign(
		id = ~ psu , 
		strata = ~ stratum , 
		weight = ~ weight , 
		data = israel_df 
	)
pew_design <- 
	update( 
		pew_design , 
		
		your_day_today =
			factor( 
				q1 , 
				levels = 1:3 ,
				labels = 
					c( 
						'a typical day' , 
						'a particularly good day' , 
						'a particularly bad day' 
					)
			) ,

		school_years = ifelse( q163b %in% 98:99 , NA , q163b ) ,
		
		age_in_years = ifelse( q146 %in% 98:99 , NA , q146 ) ,

		country_economic_situation =
			factor(
				q3 ,
				levels = 1:4 ,
				labels = 
					c( 
						'very good' , 
						'somewhat good' , 
						'somewhat bad' , 
						'very bad' 
					)
			)
	)
sum( weights( pew_design , "sampling" ) != 0 )

svyby( ~ one , ~ your_day_today , pew_design , unwtd.count )
svytotal( ~ one , pew_design )

svyby( ~ one , ~ your_day_today , pew_design , svytotal )
svymean( ~ school_years , pew_design , na.rm = TRUE )

svyby( ~ school_years , ~ your_day_today , pew_design , svymean , na.rm = TRUE )
svymean( ~ country_economic_situation , pew_design , na.rm = TRUE )

svyby( ~ country_economic_situation , ~ your_day_today , pew_design , svymean , na.rm = TRUE )
svytotal( ~ school_years , pew_design , na.rm = TRUE )

svyby( ~ school_years , ~ your_day_today , pew_design , svytotal , na.rm = TRUE )
svytotal( ~ country_economic_situation , pew_design , na.rm = TRUE )

svyby( ~ country_economic_situation , ~ your_day_today , pew_design , svytotal , na.rm = TRUE )
svyquantile( ~ school_years , pew_design , 0.5 , na.rm = TRUE )

svyby( 
	~ school_years , 
	~ your_day_today , 
	pew_design , 
	svyquantile , 
	0.5 ,
	ci = TRUE ,
	keep.var = TRUE ,
	na.rm = TRUE
)
svyratio( 
	numerator = ~ school_years , 
	denominator = ~ age_in_years , 
	pew_design ,
	na.rm = TRUE
)
sub_pew_design <- subset( pew_design , q13a %in% 1:2 )
svymean( ~ school_years , sub_pew_design , na.rm = TRUE )
this_result <- svymean( ~ school_years , pew_design , na.rm = TRUE )

coef( this_result )
SE( this_result )
confint( this_result )
cv( this_result )

grouped_result <-
	svyby( 
		~ school_years , 
		~ your_day_today , 
		pew_design , 
		svymean ,
		na.rm = TRUE 
	)
	
coef( grouped_result )
SE( grouped_result )
confint( grouped_result )
cv( grouped_result )
degf( pew_design )
svyvar( ~ school_years , pew_design , na.rm = TRUE )
# SRS without replacement
svymean( ~ school_years , pew_design , na.rm = TRUE , deff = TRUE )

# SRS with replacement
svymean( ~ school_years , pew_design , na.rm = TRUE , deff = "replace" )
svyciprop( ~ children_better_off , pew_design ,
	method = "likelihood" , na.rm = TRUE )
svyttest( school_years ~ children_better_off , pew_design )
svychisq( 
	~ children_better_off + country_economic_situation , 
	pew_design 
)
glm_result <- 
	svyglm( 
		school_years ~ children_better_off + country_economic_situation , 
		pew_design 
	)

summary( glm_result )
library(srvyr)
pew_srvyr_design <- as_survey( pew_design )
pew_srvyr_design %>%
	summarize( mean = survey_mean( school_years , na.rm = TRUE ) )

pew_srvyr_design %>%
	group_by( your_day_today ) %>%
	summarize( mean = survey_mean( school_years , na.rm = TRUE ) )

