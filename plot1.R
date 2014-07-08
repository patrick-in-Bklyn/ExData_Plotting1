# Project 1 :  exploratory data analysis
# Due Sunday July 13th 2014

#set the directory

setwd()

# Plot function

plot1 <- function(wnt = "subset.txt")
        {
        main_url <- "https://archive.ics.uci.edu/ml/machine-learning-databases/00235/"
        destfile <- "household_power_consumption.txt"
        
        
        if (wnt %in% list.files(".")){file <- read.table(wnt, header = TRUE, sep = ";", na.strings = "?");}
        else
        {
        if (destfile %in% list.files(".")) {file <- section(destfile);}
        else { start_file <- get_file(main_url, destfile); file <- section(destfile);}
        }
        
        heading = "Global Active Power"
        lab_x = "Global Active Power (kilowatts)"
        lab_y <- "Frequency"
        with(file, hist(Global_active_power, main = heading, xlab = lab_x, ylab = lab_y, col = "red"));
        par(cex.axis = 0.8);
        par(cex.axis = 0.8);
        
        dev.copy(png, file = "plot1.png", width = 480, height = 480);
        dev.off();
        
             
        
        }

# if the file doesn't exist in directories, download it and unzip.

get_file <- function(url, destfile)
        {
        library(httr)
        library(XML);
        file <- GET(url);
        doc <- htmlTreeParse(file, useInternal = TRUE)
        root <- xmlRoot(doc)
        links <- as.data.frame(xmlSApply(root, function(x) xpathApply(x, "//a/@href")))
        stem <- strsplit(links$body[[6]]["href"], "href")[1]
        new_url <- paste(url, stem, sep = "", collapse = "/");
        zip_file <- download.file(new_url, destfile = "household_cons.zip", method = "curl");

        wnt_dir <- unzip("household_cons.zip", exdir = ".");

        
        }
#read the data and extract the section we want

section <- function(x)
        {
        tble <- read.table(x, header = TRUE, sep = ";", na.strings = "?", nr = -1, skip = 0);
        
        tble$Date <- as.Date(tble$Date, format = "%d/%m/%Y");
        
        rows1 <- which(tble$Date == "2007-02-01");
        rows2 <- which(tble$Date == "2007-02-02");
        final <- rbind(tble[rows1,], tble[rows2,]);
        write.table(final, file = "./subset.txt", quote = FALSE, na = "?", sep = ";")
        return(final);
        }

