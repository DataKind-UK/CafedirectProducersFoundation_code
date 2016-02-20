calculate_support_and_info_source <- function (country) {
    possible_answers <- c("My own experimentation", "Sharing with other farmers/friends/family", 
                          "A farmer promoter visits my farm", "An extension officer visits my farm", 
                          "Visit to demonstration farm", "Radio stations", "WeFarm", "Mobile phones", "Newspapers",
                          "Internet", "Television", "Visiting a Centre of Excellence", "Buying Centre",
                          "My primary cooperative", "Being a member of a local group (women’s groups etc)",
                          "Training sessions/workshops (including FFSs)", "Training materials / leaflets", "Other (Specify)")
    temp <- data.frame(response = c(), total = c())
    for (x in seq(1, 18)) {
        temp <- rbind(temp, data.frame(
            response = possible_answers[x], 
            total = sum(ifelse(country[paste0("Q129.InformationFromWhere.O", x, collapse = "")] == 1, 1, 0))
        ))
    }
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

write.csv(left_join(s_uganda, s_kenya), file = "Q129.csv")
