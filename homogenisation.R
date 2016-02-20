uganda <- read.csv("../CafedirectProducersFoundation_data/DataPrep/UgandaClean.mapped.csv")
kenya <- read.csv("../CafedirectProducersFoundation_data/DataPrep/KenyaClean.mapped.csv")

# drop the tags from the Uganda data
# uganda <- uganda[, names(uganda)[substr(names(uganda), nchar(names(uganda)) - 4, nchar(names(uganda))) != ".Tags"]]

# drop the "Unnamed" columns
uganda <- uganda[, substr(names(uganda), 1, 8) != "Unnamed."]
kenya <- kenya[, substr(names(uganda), 1, 8) != "Unnamed."]

# replace the non-clean columns with the clean ones
cleanUgandaColumns <- names(uganda)[substr(names(uganda), nchar(names(uganda)) - 5, nchar(names(uganda))) == ".Clean"]
for (columnName in cleanUgandaColumns) {
    temp <- substr(columnName, 1, nchar(columnName) - 6)
    uganda[temp] <- uganda[paste0(c(temp, ".Clean"), collapse = "")]
    uganda <- uganda[, names(uganda) != paste0(c(temp, ".Clean"), collapse = "") ]
    uganda <- uganda[, names(uganda) != paste0(c(temp, ".FreeText"), collapse = "") ]
}

# drop any extra columns from both datasets
uganda <- uganda[, ! names(uganda) %in% setdiff(names(uganda), names(kenya))]
kenya <- kenya[, ! names(kenya) %in% setdiff(names(kenya), names(uganda))]

