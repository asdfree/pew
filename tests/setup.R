if ( .Platform$OS.type == 'windows' ) memory.limit( 256000 )

library(lodown)

pew_cat <-
	get_catalog( "pew" ,
		output_dir = file.path( getwd() ) )

# sample 50% of the records
which_records <- sample( seq( nrow( pew_cat ) ) , round( nrow( pew_cat ) * 0.50 ) )

# always sample the united states wave == 6 file
pew_cat <- unique( rbind( pew_cat[ which_records , ] , subset( pew_cat , name == "Spring 2015 Survey Data" ) ) )

lodown( "pew" , pew_cat )
