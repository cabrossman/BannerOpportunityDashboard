path <- "C:\\Users\\christopher.brossman\\Documents\\R files\\R_Functions\\AuthFunctions.R"
source(path)


end_date <- as.character(Sys.Date() - 1)
dir.create("output")
path <- paste0(getwd(),"/output/","")

OAS_data <- OAS_Rolling30_Driver()
DFP_data <- DFP_Rolling30_Driver()

all_data <- rbind(OAS_data, DFP_data)


#saveFiles&upload
print("Writing files to drive")
path <- "C:\\Users\\christopher.brossman\\Documents\\R files\\"
nam <- paste0(path,"Rolling30_",end_date,".csv")
write.csv(all_data,nam,row.names=FALSE)