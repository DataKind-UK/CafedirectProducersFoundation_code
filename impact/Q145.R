# Specify 1 for males, 2 for females
calculate_support_and_info_source <- function (country, gender = 0) {
    
    if (gender != 0) country <- country[country$Q10.Sex == gender, ]
    
    country <- columns_transformation(country, "Q145.MostValuableSources.O")

    possible_answers <- c("My own experimentation", "Sharing with other farmers/friends/family", "Guidance from farmer promoters", "Extension workers from your cooperative", "Radio stations", "WeFarm", "Mobile phones", "Newspapers", "Television", "Internet", "Centres of Excellence", "Being a member of a local group (women's groups etc)", "Training sessions/workshops (including FFSs)", "Training materials / leaflets")
    temp <- data.frame(response = c(), total = c())
    for (x in seq(1, 14)) {
        temp <- rbind(temp, data.frame(
            response = possible_answers[x], 
            total = sum(ifelse(country[paste0("Q145.MostValuableSources.O", x, collapse = "")] == 1, 1, 0))
        ))
    }
    temp$percent <- temp$total / nrow(country)
    temp$response <- as.character(temp$response)
    temp <- temp %>% arrange(response)
    return(temp)
}

s_uganda <- calculate_support_and_info_source(uganda, 1)
s_uganda <- rename(s_uganda, "Uganda (% of respondents)" = percent)
s_uganda <- select(s_uganda, -total)

s_kenya <- calculate_support_and_info_source(kenya, 1)
s_kenya <- rename(s_kenya, "Kenya (% of respondents)" = percent)
s_kenya <- select(s_kenya, -total)

write.csv(left_join(s_uganda, s_kenya), file = "Q145.csv")


