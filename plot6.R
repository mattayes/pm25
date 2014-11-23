## Packages required
library(tidyr); library(dplyr); library(lubridate); library(ggplot2); library(grid)

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

## Filter Baltimore and Los Angeles readings
los_baltimore <- nei %>%
        filter(fips == "24510" | fips == "06037")

## Filter motor vehicle (mobile on-road) SCC, inner join with los_baltimore, 
## and clean up column names
mobile <- scc %>%
        select(SCC, EI.Sector) %>%
        filter(grepl("On-Road", scc$EI.Sector))
los_baltimore <- inner_join(los_baltimore, mobile, by = c("scc" = "SCC"))
rm(mobile)
names(los_baltimore) <- tolower(names(los_baltimore))

## Find totals by year/fips, min totals by fips, and the difference
los_baltimore <- los_baltimore %>%
        group_by(year, fips) %>%
        summarize(total = sum(emissions)) %>%
        ungroup() %>%
        group_by(fips) %>%
        mutate(range = range(total))

## First Plot
p1 <- ggplot(los_baltimore, aes(year, total)) + 
        geom_line(aes(color = fips)) +
        xlab("Year") +
        ylab("Emissions (tons)") +
        ggtitle(expression("Mobile On-Road PM"[2.5]*" Emissions by City")) +
        scale_colour_discrete(name = "City",
                              breaks = c("06037", "24510"),
                              labels = c("Los Angeles", "Baltimore City")
                              )

## Second Plot
p2 <- ggplot(los_baltimore, aes(year, diffmin)) +
        geom_bar(stat = "identity", aes(fill = fips), 
                 position = position_dodge(), color = "black") +
        xlab("Year") +
        ylab("Difference from Minimum Value (tons)") +
        ggtitle("Baltimore: Steady Decline, Los Angeles: Erratic Swings")  +
        scale_fill_discrete(name = "City",
                              breaks = c("06037", "24510"),
                              labels = c("Los Angeles", "Baltimore City")
                              )

## Putting it together
png("plot6.png")
grid.newpage()
pushViewport(viewport(layout = grid.layout(2, 1)))
## Helper function
vplayout <- function(x, y)
        viewport(layout.pos.row = x, layout.pos.col = y)
print(p1, vp = vplayout(1, 1))
print(p2, vp = vplayout(2, 1))
dev.off()