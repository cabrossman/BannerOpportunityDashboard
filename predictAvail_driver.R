#Get all the necessary functions
path <- '~/R files/R_Functions/AuthFunctions.R'
source(path)


# dir.create("output")
path <- paste0(getwd(),"/output/","")
end_date <- as.character(Sys.Date() - 1)
start_date <- as.character(Sys.Date()-8)



##########get dashboard forecast data
dfpPredictAvail <- DFP_dashboardPredictAvail()


oasPredictAvail <- OAS_predictAvail()

predictAvail <- rbind(dfpPredictAvail,oasPredictAvail)

#saveFiles&uploadToGooglDrive
print("Writing files to drive")
nam <- paste0(path,"predictAvail_",end_date,".csv")
write.csv(predictAvail,nam,row.names=FALSE)