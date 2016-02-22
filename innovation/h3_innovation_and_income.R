# If any of these libraries aren't installed:
# install.packages( c('dplyr', 'reshape2', 'ggplot2', 'readr' ) )
library(dplyr)
library(reshape2)
library(ggplot2)
library(readr)

# Hypothesis 3: Farmers who innovate and adopt good ideas earn more income

# I'm taking "innovate and adopt good ideas" to mean the number of techs trained and used
#  and "more income" to mean a higher income group, as defined in the "enhanced" data files.

# Get the data
# (some unfixed date parsing problems which don't affect these results)
kenya  <- read_delim( 'data/KenyaEnhanced.csv', delim=',' )
uganda <- read_delim( 'data/UgandaEnhanced.csv', delim=',' )


## KENYA: Plot income group against number of techs used
kenya_training_vs_earnings <- kenya %>%
        group_by( n_techs_trained_and_used,earnings_group ) %>%
        summarise( COUNT = n() ) %>%
        ungroup %>%
        ggplot( aes( x=n_techs_trained_and_used, y=earnings_group ) ) +
            geom_point(aes(size = COUNT)) +
            scale_x_discrete() +
            scale_y_discrete() +
            xlab( "Number of techs trained and used ") +
            ylab( "Earnings group" ) +
            labs(title = "KENYA: Training and earnings group") +
            theme_bw() 
            
# save plot to file
ggsave( 'kenya_training_vs_earnings.png' )


## UGANDA: Plot income group against number of techs used
uganda_training_vs_earnings <- uganda %>%
        group_by( n_techs_trained_and_used,earnings_group ) %>%
        summarise( COUNT = n() ) %>%
        ungroup %>%
        ggplot( aes( x=n_techs_trained_and_used, y=earnings_group ) ) +
            geom_point(aes(size = COUNT)) +
            scale_x_discrete() +
            scale_y_discrete() +
            xlab( "Number of techs trained and used ") +
            ylab( "Earnings group" ) +
            labs(title = "UGANDA: Training and earnings group") +
            theme_bw()
            
# save plot to file
ggsave( 'uganda_training_vs_earnings.png' )
