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

## Filter Baltimore City readings
mobile_balt <- nei %>%
        filter(fips == "24510")

## Find motor vehicle (aka mobile on-road) SCC, inner join with nei, and clean up column names
mobile <- scc %>%
        select(SCC, EI.Sector) %>%
        filter(grepl("On-Road", scc$EI.Sector))
mobile <- inner_join(mobile_balt, mobile, by = c("scc" = "SCC"))
names(mobile) <- tolower(names(mobile))

## Group by year, summarize
mobile_total <- mobile %>%
        group_by(year) %>%
        summarize(total = sum(emissions))

## Plot
png("plot5.png")
ggplot(mobile_total, aes(year, total)) +
        geom_point() +
        geom_smooth(method = "lm") +
        coord_cartesian(ylim = c(0, 600)) +
        xlab("Year") +
        ylab("Emissions (tons)") +
        ggtitle(expression("Total Emissions of PM"[2.5]*" from Mobile On-Road Sources"))
dev.off()