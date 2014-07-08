# Project 1 :  exploratory data analysis
# Due Sunday July 13th 2014

#set the directory

setwd()

# get the website address, unzip the file

plot4 <- function(wnt = "subset.txt")
        {
        main_url <- "https://archive.ics.uci.edu/ml/machine-learning-databases/00235/"
        destfile <- "household_power_consumption.txt"
        
        
        if (wnt %in% list.files(".")){file <- read.table(wnt, header = TRUE, sep = ";", na.strings = "?");}
        else
        {
        if (destfile %in% list.files(".")) {file <- section(destfile);}
        else { file <- section(get_file(main_url, destfile));}
        }
        par(mfrow = c(2,2));
        file$Date <- as.Date(file$Date);
        
        with(file, plot(Global_active_power, type = "l", lty = 1, lwd = 1, xlab = "", ylab = "",yaxt = 'n', xaxt = 'n'));
        
        axis(1, at = c(1, nrow(file)/2, nrow(file)), labels = c("Thu", "Fri", "Sat"), cex.axis = 0.9);
        
        axis(2, cex.axis = 0.8);
        mtext("Global Active Power (kilowatts)", side = 2, line = 2.2, cex = 0.6)
        
        with(file, plot(Voltage, type = "l", lty = 1, lwd = 1, xlab = "", ylab = "",yaxt = 'n', xaxt = 'n'));
        
        axis(1, at = c(1, nrow(file)/2, nrow(file)), labels = c("Thu", "Fri", "Sat"), cex.axis = 0.9);
        
        axis(2, cex.axis = 0.8);
        mtext("Voltage", side = 2, line = 2.2, cex = 0.6);
        
        
        with(file, plot(file$Sub_metering_1, type = "l", ylim = c(min(file$Sub_metering_1), max(file$Sub_metering_1)), lwd = 0.9, xlab = "", ylab = "",yaxt = 'n', xaxt = 'n', col = "black"));
        lines(file$Sub_metering_2, type = "l", col = "red");
        lines(file$Sub_metering_3, type = "l", col = "blue");
        axis(1, at = c(1, nrow(file)/2, nrow(file)), labels = c("Thu", "Fri", "Sat"), cex.axis = 0.8);
        axis(2, cex.axis = 0.6);
        mtext("Energy sub metering", side = 2, line = 2.2, cex = 0.6);
        
        
        
        with(file, plot(Global_reactive_power, type = "l", lty = 1, lwd = 1, xlab = "", ylab = "",yaxt = 'n', xaxt = 'n'));
        
        axis(1, at = c(1, nrow(file)/2, nrow(file)), labels = c("Thu", "Fri", "Sat"), cex.axis = 0.9);
        
        axis(2, cex.axis = 0.6);
        mtext("Global Reactive Power (kilowatts)", side = 2, line = 2.2, cex = 0.6);
        

        
        dev.copy(png, file = "plot4.png", width = 480, height = 480);
        dev.off();
        
         
        
        }

# If the file is not in the working dir, download and unzip it.

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
        wnt_dir <- download.file(new_url, destfile = "./household_cons.zip", method = "curl");
        want_fles <- unzip("./househols_cons.zip", exdir =".", overwrite = TRUE);
        wnt_tble <- read.table(want_files, header = TRUE, sep = ";", na.strings = "?", nr = -1, skip = 0);
        
}

#extract date sections of the file.

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

