# melt dataset
enzMlt <- melt(enzy, id = names(enzy)[which(!(names(enzy) %in% c("cello.act", "gluco.act", "nag.act", "phos.act")))])

# Ring summary table & mean
RngSmmryTbl <- dlply(enzMlt, .(variable), function(x) CreateTable(x, fac = "ring", digit = 4, nsmall = 4))
RngMean <- ddply(enzMlt, .(date, co2, ring, variable), summarise, value = mean(value, na.rm = TRUE)) 

# treat summary table $ mean
TrtSmmryTbl <- dlply(RngMean, .(variable), function(x) CreateTable(x, fac = "co2",  digit = 4, nsmall = 4))

## create xcel workbook ##
wb <- createWorkbook()

# worksheet for rowdata
sheet <- createSheet(wb,sheetName="row_data")
addDataFrame(enzy, sheet, showNA=TRUE, row.names=FALSE, characterNA="NA")

# worksheets for ring summary
shnames <- paste("Ring_mean.",c("Cellobiohydrolase", "Glucosidase", "NAG", "Acid_phsphatase"),
                 sep="")
l_ply(1:4, function(x) crSheet(sheetname = shnames[x], dataset = RngSmmryTbl[[x]]))

# worksheets for temp trt summary
shnames <- paste("CO2_mean.", c("Cellobiohydrolase", "Glucosidase", "NAG", "Acid_phsphatase"),
                 sep = "")
l_ply(1:4, function(x) crSheet(sheetname = shnames[x], dataset = TrtSmmryTbl[[x]]))

#save file
saveWorkbook(wb,"Output/Table/FACE_Enzyme.xlsx")
