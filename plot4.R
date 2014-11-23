## Packages required
library(tidyr); library(dplyr); library(lubridate); library(ggplot2)

## Download data
if(!file.exists("./data")){
        dir.create("./data")
        fileUrl <- "https://d396qusza40orc.cloudfront.net/exdata%2Fdata%2FNEI_data.zip"
        download.file(fileUrl, "./data/zip.zip", method = "curl")
        rm(fileUrl)
        unzip("./data/zip.zip", exdir = "./data")
}

## Read data
nei <- tbl_df(readRDS("./data/summarySCC_PM25.rds"))
scc <- tbl_df(readRDS("./data/Source_Classification_Code.rds"))

## Clean up data
names(nei) <- tolower(names(nei))
nei <- nei %>%
        select(-pollutant) %>%
        mutate(fips = factor(fips),
               scc = factor(scc),
               type = factor(tolower(type)),
               year = parse_date_time(year, "%Y")
        )

## Find coal combustion SCC, inner join with nei, and clean up column names
coal <- scc %>%
        select(SCC, EI.Sector) %>%
        filter(grepl("[Cc]oal", scc$EI.Sector))
coal <- inner_join(nei, coal, by = c("scc" = "SCC"))
names(coal) <- tolower(names(coal))

## Group by year, summarize
coal_total <- coal %>%
        group_by(year) %>%
        summarize(total = sum(emissions))

## Plot
png("plot4.png")
ggplot(coal_total, aes(year, total)) +
        geom_point() +
        geom_smooth(method = "lm") +
        xlab("Year") +
        ylab("Emissions (tons)") +
        ggtitle(expression("Total Emissions of PM"[2.5]*" from Coal Combustion-Related Sources"))
dev.off()