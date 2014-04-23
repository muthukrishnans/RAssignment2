pollutantmean <- function(directory, pollutant, id = 1:332) {
    
    files <- id
    print(files)
    fileCount <- length(files)
    fileCount
    globalTitle <- NULL
    for( i in files)
    {
        print(i)
        filenames <- sprintf("%03d.csv", i)
        filename <- paste("./", directory, "/", filenames, sep = "")
        print(filename)
		title <- read.csv(filename, header = TRUE, sep = ",", quote = "\"", dec = ".", nrow = 1 )	
        len <- length(colnames(title))	
        colnumber = grep(pollutant, c(colnames(title)))
        if (colnumber > 1)
        {
            format <- c(rep('NULL', colnumber - 1), 'numeric', rep('NULL', len - colnumber))
            title <- read.csv(filename, header = TRUE, sep = ",", quote = "\"", dec = ".", colClasses = format, skip = 1)
            print(nrow(title))
            title <- na.omit(title)
            print(nrow(title)) 
            globalTitle <- rbind (globalTitle, title)
            print(nrow(globalTitle))
            
        }
        else
        {
            format <- c('numeric', rep('NULL', len - colnumber))
            title <- read.csv(filename, header = TRUE, sep = ",", quote = "\"", dec = ".", colClasses = format, skip = 1)		
            print(length(title))
            title <- na.omit(title)
            print(length(title))
		}
	}
    totalMean = mean(globalTitle[[1]], trim = 0)
    print(totalMean)
}
