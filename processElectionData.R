source("functions.R")

# An Excel file containing the election results was downloaded from
# http://www.infoelectoral.interior.es/docxl/04_201105_1.zip.
#
# The XLSX file has been processed with xlsx2csv 
# (https://github.com/dilshod/xlsx2csv) using the following command:
# $ xlsx2csv 04_201105_1.xlsx -s 1 04_201105_1_sheet1.csv. 
#
# The resulting CSV file was then edited to:
#   1. Remove the first 5 rows
#   2. Remove the last row
#   3. Add a comment ('#') on the row corresponding to Vilella Alta (La), as
#      nobody voted there.
file_path <- "~/Dropbox/data/elecciones20112015/04_201105_1_sheet1.csv"

# "Big" CSV file, read data only if not already read
if(!exists("elections")) {
    elections <- readRawData(file_path)
}

# Process each town
election_processed <- lapply(1:nrow(elections), function(t) {
    res <- getTownResults(elections[t, ])
    res$town <- gsub("[\\s\\n]+$", "", elections[t, ]$Nombre.de.Municipio, 
                     perl = TRUE)
    res$province <- gsub("[\\s\\n]+$", "", elections[t, ]$Nombre.de.Provincia, 
                         perl = TRUE)
    return(res)
})

election_processed <- do.call(rbind, election_processed)

save(election_processed, 
     file = "~/Dropbox/data/elecciones20112015/elections2011_processed.Rda")
