columns_transformation <- function (source_table, column_name_prefix) {
    target_columns <- names(source_table)[grepl(column_name_prefix, names(source_table))]
    max_options <- max(sapply(target_columns, function (x) { return(as.numeric(substr(x, nchar(column_name_prefix) + 1, nchar(x)))) }))
    for (row_no in seq(1, nrow(source_table))) {
        responses <- table(data.matrix(source_table[row_no, sapply(seq(1, max_options), function (x) { return(paste0(column_name_prefix, as.character(x), collapse = "")) }) ]))
        for (x in seq(1, max_options)) {
            source_table[row_no, paste0(column_name_prefix, as.character(x), collapse = "")] <- ifelse(sum(names(responses) == as.character(x)) > 0, 1, -1)
        }
    }
    return(source_table)    
}

# EXAMPLE
# Read the original...
uganda <- read.csv("source-data-aka-usb-stick/UgandaEnhanced.csv")
# ... and transform
write.csv(columns_transformation(uganda, "Q145.MostValuableSources.O"), file = "UgandaClean.mapped.Q145_fixed.csv")

kenya <- read.csv("source-data-aka-usb-stick/KenyaEnhanced.csv")
write.csv(columns_transformation(kenya, "Q145.MostValuableSources.O"), file = "KenyaClean.mapped.Q145_fixed.csv")
                    