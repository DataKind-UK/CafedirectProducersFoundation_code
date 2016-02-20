calculate_support_and_info_source <- function (country) {
    responses <- table(data.matrix(country[, c("Q145.MostValuableSources.O1", "Q145.MostValuableSources.O2", "Q145.MostValuableSources.O3", "Q145.MostValuableSources.O4", "Q145.MostValuableSources.O5", "Q145.MostValuableSources.O6", "Q145.MostValuableSources.O7", "Q145.MostValuableSources.O8", "Q145.MostValuableSources.O9", "Q145.MostValuableSources.O10", "Q145.MostValuableSources.O11", "Q145.MostValuableSources.O12", "Q145.MostValuableSources.O13", "Q145.MostValuableSources.O14")]))
    temp <- data.frame(response = c(), total = c())
    temp <- rbind(temp, data.frame(response = "My own experimentation", total = responses[names(responses) == "1"]))
    temp <- rbind(temp, data.frame(response = "Sharing with other farmers/friends/family", total = responses[names(responses) == "2"]))
    temp <- rbind(temp, data.frame(response = "Guidance from farmer promoters", total = responses[names(responses) == "3"]))
    temp <- rbind(temp, data.frame(response = "Extension workers from your cooperative", total = responses[names(responses) == "4"]))
    temp <- rbind(temp, data.frame(response = "Radio stations", total = responses[names(responses) == "5"]))
    temp <- rbind(temp, data.frame(response = "WeFarm", total = responses[names(responses) == "6"]))
    temp <- rbind(temp, data.frame(response = "Mobile phones", total = responses[names(responses) == "7"]))
    temp <- rbind(temp, data.frame(response = "Newspapers", total = responses[names(responses) == "8"]))
    temp <- rbind(temp, data.frame(response = "Television", total = responses[names(responses) == "9"]))
    temp <- rbind(temp, data.frame(response = "Internet", total = responses[names(responses) == "10"]))
    temp <- rbind(temp, data.frame(response = "Centres of Excellence", total = responses[names(responses) == "11"]))
    temp <- rbind(temp, data.frame(response = "Being a member of a local group (women's groups etc)", total = responses[names(responses) == "12"]))
    temp <- rbind(temp, data.frame(response = "Training sessions/workshops (including FFSs)", total = responses[names(responses) == "13"]))
    temp <- rbind(temp, data.frame(response = "Training materials / leaflets", total = responses[names(responses) == "14"]))
    temp$percent <- temp$total / nrow(country)
    temp$response <- as.character(temp$response)
    temp <- temp %>% arrange(response)
    return(temp)
}

s_uganda <- calculate_support_and_info_source(uganda)
s_uganda <- rename(s_uganda, "Uganda (% of respondents)" = percent)
s_uganda <- select(s_uganda, -total)

s_kenya <- calculate_support_and_info_source(kenya)
s_kenya <- rename(s_kenya, "Kenya (% of respondents)" = percent)
s_kenya <- select(s_kenya, -total)

write.csv(left_join(s_uganda, s_kenya), file = "Q145.csv")


