calculate_support_and_info_source <- function (country) {
    temp <- data.frame(response = c(), total = c())
    temp <- rbind(temp, data.frame(response = "My own experimentation", total = sum(ifelse(country$Q129.InformationFromWhere.O1 == 1, 1, 0))))
    temp <- rbind(temp, data.frame(response = "Sharing with other farmers/friends/family", total  = sum(ifelse(country$Q129.InformationFromWhere.O2 == 1, 1, 0))))
    temp <- rbind(temp, data.frame(response = "A farmer promoter visits my farm", total  = sum(ifelse(country$Q129.InformationFromWhere.O3 == 1, 1, 0))))
    temp <- rbind(temp, data.frame(response = "An extension officer visits my farm", total  = sum(ifelse(country$Q129.InformationFromWhere.O4 == 1, 1, 0))))
    temp <- rbind(temp, data.frame(response = "Visit to demonstration farm", total  = sum(ifelse(country$Q129.InformationFromWhere.O5 == 1, 1, 0))))
    temp <- rbind(temp, data.frame(response = "Radio stations", total  = sum(ifelse(country$Q129.InformationFromWhere.O6 == 1, 1, 0))))
    temp <- rbind(temp, data.frame(response = "WeFarm", total  = sum(ifelse(country$Q129.InformationFromWhere.O7 == 1, 1, 0))))
    temp <- rbind(temp, data.frame(response = "Mobile phones", total  = sum(ifelse(country$Q129.InformationFromWhere.O8 == 1, 1, 0))))
    temp <- rbind(temp, data.frame(response = "Newspapers", total  = sum(ifelse(country$Q129.InformationFromWhere.O9 == 1, 1, 0))))
    temp <- rbind(temp, data.frame(response = "Internet", total  = sum(ifelse(country$Q129.InformationFromWhere.O10 == 1, 1, 0))))
    temp <- rbind(temp, data.frame(response = "Television", total  = sum(ifelse(country$Q129.InformationFromWhere.O11 == 1, 1, 0))))
    temp <- rbind(temp, data.frame(response = "Visiting a Centre of Excellence", total  = sum(ifelse(country$Q129.InformationFromWhere.O12 == 1, 1, 0))))
    temp <- rbind(temp, data.frame(response = "Buying Centre", total  = sum(ifelse(country$Q129.InformationFromWhere.O13 == 1, 1, 0))))
    temp <- rbind(temp, data.frame(response = "My primary cooperative", total  = sum(ifelse(country$Q129.InformationFromWhere.O14 == 1, 1, 0))))
    temp <- rbind(temp, data.frame(response = "Being a member of a local group (women’s groups etc)", total  = sum(ifelse(country$Q129.InformationFromWhere.O15 == 1, 1, 0))))
    temp <- rbind(temp, data.frame(response = "Training sessions/workshops (including FFSs)", total  = sum(ifelse(country$Q129.InformationFromWhere.O16 == 1, 1, 0))))
    temp <- rbind(temp, data.frame(response = "Training materials / leaflets", total  = sum(ifelse(country$Q129.InformationFromWhere.O17 == 1, 1, 0))))
    temp <- rbind(temp, data.frame(response = "Other (Specify)", total  = sum(ifelse(country$Q129.InformationFromWhere.O18 == 1, 1, 0))))
    temp$percent <- temp$total / nrow(country)
    temp$response <- as.character(temp$response)
    temp <- temp %>% arrange(response)
    return(temp)
}

s_uganda <- calculate_support_and_info_source(uganda)
s_uganda <- rename(s_uganda, uganda_percent = percent)
s_uganda <- select(s_uganda, -total)

s_kenya <- calculate_support_and_info_source(kenya)
s_kenya <- rename(s_kenya, kenya_percent = percent)
s_kenya <- select(s_kenya, -total)

write.csv(left_join(s_uganda, s_kenya), file = "Q129.csv")


