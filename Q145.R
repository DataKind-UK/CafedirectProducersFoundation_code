# Specify 1 for males, 2 for females
calculate_support_and_info_source <- function (country, gender = 0) {
    
    if (gender != 0) country <- country[country$Q10.Sex == gender, ]
    
    # this creates a summary of all values appearing in the specified columns and
    # the number of occurrencies
    responses <- table(data.matrix(country[ , sapply(seq(1, 14), function (x) { return(paste0("Q145.MostValuableSources.O", as.character(x), collapse = "")) }) ]))
    
    temp <- data.frame(response = c(), total = c())
    option_position <- 0
    for (option in c("My own experimentation", "Sharing with other farmers/friends/family", "Guidance from farmer promoters", "Extension workers from your cooperative", "Radio stations", "WeFarm", "Mobile phones", "Newspapers", "Television", "Internet", "Centres of Excellence", "Being a member of a local group (women's groups etc)", "Training sessions/workshops (including FFSs)", "Training materials / leaflets")) {
        option_position <- option_position + 1
        temp <- rbind(temp, data.frame(response = option, total = ifelse(sum(names(responses) == as.character(option_position)) > 0, responses[names(responses) == as.character(option_position)], 0)))
    }
    
    temp$percent <- temp$total / nrow(country)
    temp$response <- as.character(temp$response)
    temp <- temp %>% arrange(response)

    return(temp)
}

uganda <- read.csv("impact/UgandaClean.mapped.csv")
kenya <- read.csv("impact/KenyaClean.mapped.csv")

s_uganda <- calculate_support_and_info_source(uganda)
s_uganda <- rename(s_uganda, "Uganda (% of respondents)" = percent)
s_uganda <- select(s_uganda, -total)

s_kenya <- calculate_support_and_info_source(kenya)
s_kenya <- rename(s_kenya, "Kenya (% of respondents)" = percent)
s_kenya <- select(s_kenya, -total)

write.csv(left_join(s_uganda, s_kenya), file = "Q145.csv")


