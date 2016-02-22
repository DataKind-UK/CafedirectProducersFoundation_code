library(dplyr)
library(reshape2)
library(ggplot2)

# Hypothesis 4: Farmers who innovate and adopt good ideas have more diversified livelihoods. 

# I'm taking "innovate and adopt good ideas" to mean the number of techs trained and used
#  and "more diversified livelihoods" to mean the number of income streams.

# Get the data
# (some unfixed date parsing problems which don't affect these results)
kenya  <- read_delim( 'data/KenyaEnhanced.csv', delim=',' )
uganda <- read_delim( 'data/UgandaEnhanced.csv', delim=',' )


## KENYA: Plot number of income streams against number of techs used
kenya_training_vs_streams <- kenya %>%
        group_by( n_techs_trained_and_used,`Streams (Cat)` ) %>%
        summarise( COUNT = n() ) %>%
        ungroup %>%
        ggplot( aes( x=n_techs_trained_and_used, y=`Streams (Cat)` ) ) +
            geom_point( aes(size = COUNT ) ) +
            scale_x_discrete() +
            scale_y_discrete() +
            xlab( "Number of techs trained and used") +
            ylab( "Number of income streams" ) +
            labs(title = "KENYA: Training and income diversification") +
            theme_bw()  
            
# save plot to file
ggsave( 'kenya_training_vs_streams.png' )


## UGANDA: Plot number of income streams against number of techs used
uganda_training_vs_streams <- uganda %>%
        group_by( n_techs_trained_and_used, `Streams (Cat)` ) %>%
        summarise( COUNT = n() ) %>%
        ungroup %>%
        ggplot( aes( x=n_techs_trained_and_used, y=`Streams (Cat)` ) ) +
            geom_point( aes(size = COUNT ) )+
            scale_x_discrete() +
            scale_y_discrete() +
            xlab( "Number of techs trained and used" ) +
            ylab( "Number of income streams" ) +
            labs(title = "UGANDA: Training and income diversification") +
            theme_bw()
            
# save plot to file
ggsave( 'uganda_training_vs_streams.png' )
