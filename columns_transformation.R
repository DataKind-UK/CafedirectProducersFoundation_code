columns_transformation <- function (source_table, column_name_prefix, max_options) {
    temp <- source_table
    for (row_no in seq(1, nrow(temp))) {
        responses <- table(data.matrix(country[row_no, sapply(seq(1, max_options), function (x) { return(paste0(column_name_prefix, as.character(x), collapse = "")) }) ]))
        for (x in seq(1, max_options)) {
            temp[row_no, paste0(column_name_prefix, as.character(x), collapse = "")] <- ifelse(sum(names(responses) == as.character(x)) > 0, 1, -1)
        }
    }
    return(temp)    
}

# EXAMPLE
# Read the original...
uganda <- read.csv("impact/UgandaClean.mapped.csv")
# ... and transform
uganda_new <- columns_transformation(uganda, "Q145.MostValuableSources.O", 14)
