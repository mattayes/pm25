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

## Filter Baltimore City readings, group by year and type, and summarize
baltimore_type <- nei %>%
        filter(fips == "24510") %>%
        group_by(year,type) %>%
        summarize(total = sum(emissions))

## Plot
png("plot3.png")
ggplot(baltimore_type, aes(year, total)) + 
        geom_line(aes(color = type)) +
        xlab("Year") +
        ylab("Emissions (tons)") +
        ggtitle(expression("PM"[2.5]*" Emissions by Type in Baltimore City, Maryland")) +
        scale_colour_discrete(name = "Type of Source",
                              breaks = c("non-road", "nonpoint", "on-road", "point"),
                              labels = c("Non-Road", "Non-Point", "On-Road", "Point")
                              )
dev.off()