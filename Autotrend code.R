start_time <- Sys.time()

#install.packages("pacman") #if necessary
pacman::p_load(officer, magrittr,  formattable,  httr, zoo, ggplot2)

#Configure proxy and working directory
set_config(use_proxy(#Proxy Infor Here))
setwd("P:\\API\\LSID")

Vectors<-read.csv("Vector.csv")
Vectors$Vector<-substring(Vectors$Vector,2)


####Prepare dictionaries####

#Frequency
freq<-data.frame("code"=c(1,2,4,6,7,9:21), "def"=c(365,52,26,12,6,4,3,2,1,0.5,0.33,0.25,0.2,0.1, 1, 4, 12 ,365))

#Scalar
scal<-data.frame("code"=0:9, "def"=c("","X10","X100","k","k X10", "k X100", "M", "M X10", "M X100", "B"))

#UOM
uom<-data.frame("code"=c(1:361, 888), "def"=c("1981=100", "198203=100", "1986=100", "198812=100", "1992 constant dollars", "1992 constant dollars per square kilometre", "1992=100", "199412=100", "1996=100", "199712=100",
                                              "1997=100", "1999=100", "2000=100", "2002 constant dollars","2002/2003 constant dollars" ,"200212=100", "2002=100", "2007 constant dollars", "200704=100", "2007=100",
                                              "2010=100", "201104=100", "2012 constant dollars", "2012/01=100", "2013=100", "access lines", "accidents", "ac", "age-sex-standardized rate per 100,000 population", "age-standardized rate per 100,000 population",
                                              "agreements", "average hours per day", "bankruptcies", "bbl", "bbl/day", "benefit periods", "birds", "births", "blocks", "fbm",
                                              "bricks", "bsh", "bsh/ac", "businesses", "calves", "Canada=100", "CAD", "CAD/cwt", "CAD per unit of foreign currency", "carriers",
                                              "cars", "cassettes", "cattle", "¢/doz", "¢", "¢/m^3", "¢/l", "¢/lb", "¢ per pound of butter fat", "chained (2002) dollars",
                                              "chained (2002) dollars per hour", "chained (2007) dollars per thousands", "chained (2007) dollars per hour", "chicks", "children", "cigarettes", "cigars", "claims", "corporations", "corporations",
                                              "crude rate per 100,000 population", "m^3", "m^3 dry", "yd^3", "current dollars", "customers", "days", "°C", "discs", "$/100lb",
                                              "$", "$/1.18kg", "$/10kg", "$/10l", "$ per 10 x 400g", "$/10,000ft", "$/15g", "$/2kg", "$/2.5kg", "$/20kg",
                                              "$/20l", "$/205l", "$/22.7l", "$/25kg", "$/3kg", "$/3.3l", "$/4l", "$/4.45l", "$/5l", "$/8l",
                                              "$/9l", "$/9000ft", "$/9.5l", "$/bsh", "$/carton", "$/doz", "$/h", "$/cwt", "$/cwt", "$/cwt",
                                              "$/cwt", "$ per cwt of milk", "$/kg", "$/kl", "$/km", "$/l", "$/mt", "$/person", "$/lb", "$/t",
                                              "$/t", "$/t", "dollars per unit of real GDP", "dollars (1972=100)", "dollars (1981=100)", "dollars (1982=100)", "dollars (1986=100)", "dollars (1992)", "doz", "dozens per person per year",
                                              "duration", "eggs", "employees", "employees", "enterprises", "establishments", "evenings", "families", "firms", "foreign-born persons",
                                              "full-time equivalent", "gal", "GJ", "GJ per thousand current dollars of production", "Gl", "GWh", "g", "head", "ha", "high 95% confidence interval",
                                              "hogs", "h", "h X1000", "households", "cwt", "cwt", "cwt", "cwt/ac", "cwt per harvested acre", "index",
                                              "index (1926=100)","index (1948=100 / 1968=100)", "index (1961=100)", "index (1967=100)", "index (1971=100)","index (1972=100)", "index (1975=1000)","index (1977=100)","index (1981=100)","index (1982-84=100)",
                                              "index (1986=100)","index (1990=100)","index (1992=100)","index (199712=100)","index (1997=100)","index (2000=1000)","index (2001=100)","index (2002=100)","index (2003=100)","index (2006=100)",
                                              "index (2007=100)","index (2008=100)","index (2009=100)","index (2010=100)", "index (2011=100)","index (2013=100)","index (82-90=100)","index (combined city average=100)", "index (1992=100)", "jobs",
                                              "kcal", "kg", "kg/ha","kg per person per year", "kl", "km", "km/h", "kt", "kWh", "kW",
                                              "layers", "lines", "l", "l (absolute alcohol)", "l per person per year", "low 95% confidence interval", "mean number", "Ml", "Mt", "MW",
                                              "message", "metric bundles", "metric rolls", "mt", "metric units", "??g", "mg", "mm", "minutes", "months",
                                              "month/day", "national currency per CAD", "number", "niacin equivalent", "nights", "X1000", "farms reporting", "visits", "per km^2", "other British and foreign-born personnes",
                                              "other british-born persons", "oven-dry mt", "pairs", "passenger-kilometres", "passenger-kilometres", "passengers", "passengers", "passengers-miles", "%", "% change (1986=100)",
                                              "% change (1992=100)", "%", "% of gross domestic income", "% of gross domestic product", "% of households", "percentage share", "person-trips", "person-visits", "persons", "pj",
                                              "point", "policies", "poults", "lb", "lb of milk", "lb/ac", "rate", "rate per 1,000 births", "rate per 1,000 legally married females", "rate per 1,000 legally married males",
                                              "rate per 1,000 live births", "rate per 1,000 males", "rate per 1,000 marriages", "rate per 1,000 total births", "rate per 100,000 population", "rate per 1,000 females", "rate per 1,000 population", "rates per 1,000 unmarried females", "rates per 1,000 males", "ratio",
                                              "ratio", "ratio", "records", "retinol equivalent", "seat-kilometres", "shares", "shares", "shelters", "ft^2", "km^2",
                                              "m^2", "tapes", "TJ", "$ X1000", "tonne-kilometres", "tonnes-miles", "t", "t", "t of oil equivalent", "t per thousand current dollars of production",
                                              "t", "t", "t/ac", "t oz", "twenty foot equivalent units", "USD per unit of real GDP", "USD", "USD", "USD/CAD", "units",
                                              "vehicle-kilometres", "vehicles", "weeks", "weights","weights (1986=100)", "weights (1997=100)", "women", "years", "$ (2002)", "??g/l",
                                              "minutes per day", "ng/ml", "nmol/l", "2013 constant dollars", "dollars per head", "qt", "$/1000m^3", "l per person per day", "twenty feet equivalent units", "GJ/1000m^3",
                                              "2007 chained dollars", "2014 constant dollars", "km^3", "(2015=100)", "ct", "gr", "hl", "kg of named substance", "air dry kilograms", "l of pure alcohol",
                                              "l", "m^2", "m^3", "m", "MWh", "packages", "m^3 X1000", "mt", "mt dry air", "doz",
                                              "g", "kg", "blank", "pairs", "2015 constant dollars", "(2014=100)", "index (201612=100)", "index (2012=100)", "available seat-kilometres", "2016 constant dollars",
                                              "index (2014=100)", "ng/l", "µmol/mmol", "µmol/l", "mg/l", "µg/m^3", "M.ft.b.m", "basis points", "$/g", "cm",
                                              "kg/m^2","" ))
####Functions####

#Function to extract series names
SeriesInfo<-function(vec,latestN){
  
  df<-data.frame(matrix(ncol=3,nrow=latestN))
  names(df)<-c("Series","uom")
  
  #POST URL
  address<-"https://www150.statcan.gc.ca/t1/wds/rest/getSeriesInfoFromVector"
  
  #POST BODY
  payload<-data.frame(vectorId=vec)
  
  #Execute API query
  res<-POST(url=address,body=payload,encode="json")
  
  #Return query result
  res<-content(res)
  
  #Obtain full series name
  df$Series<-res[[1]]$object$SeriesTitleEn
  
  #Keep only relevant part of name and print it
  df$Series<-strsplit(df$Series, ";\\s*(?=[^;]+$)", perl=TRUE)[[1]][2]
  
  #uom ID
  df$uomnum<-res[[1]]$object$memberUomCode
  df$uom<-uom[grepl(df$uomnum, uom$code), "def"][[1]]
  
  #Frequency
  df$Freq<-res[[1]]$object$frequencyCode
  df$Freq<-latestN*freq[grepl(df$Freq, freq$code), "def"][[1]]
  
  return(df)
}

#Function to extract the table name from vector (for slide metadata)
CubeMetadata<-function(pid){
  
  
  #POST URL
  address<-"https://www150.statcan.gc.ca/t1/wds/rest/getCubeMetadata"
  
  #POST BODY
  payload<-data.frame(productId=pid)
  
  #Execute API query
  res<-POST(url=address,body=payload,encode="json")
  
  #Return query result
  res<-content(res)
  
  #Print CubeID
  return(res[[1]]$object$cubeTitleEn)
}

#Function to extract data from vectors for the latest N periods (unindexed)
datN<-function(vlist,Observations){
  
  df<-data.frame()
  for(i in 1:(length(vlist)-1)){
    Obs<-SeriesInfo(vlist[[i]], Observations)
    
    address <- 'https://www150.statcan.gc.ca/t1/wds/rest/getDataFromVectorsAndLatestNPeriods'
    #POST BODY
    payload <-data.frame(vectorId=vlist[[i]], latestN=Obs$Freq[1])
    
    #Execute API query
    response<-POST(url=address, body=payload, encode="json")
    
    #Return query result
    res<-content(response)
    
    #Return to a dataframe
    framelist<-list()
    for(j in 1:as.integer(Obs$Freq[1])){
      frame<-data.frame(res[[1]]$object$vectorDataPoint[[j]]$refPer, res[[1]]$object$vectorDataPoint[[j]]$value, res[[1]]$object$vectorDataPoint[[j]]$frequencyCode, res[[1]]$object$vectorDataPoint[[j]]$scalarFactorCode)
      names(frame)<-c( "Date", "Value", "freq", "scalar")
      frame$freq<-freq[grepl(frame$freq, freq$code), "def"][1]
      frame$scalar<-scal[grepl(frame$scalar, scal$code), "def"][1]
      framelist[[j]]<-frame
    }
    df1<- do.call(rbind, framelist)
    
    df2<-SeriesInfo(vlist[[i]], Observations)
    df1<-cbind(df1,df2)
    df<-rbind(df, df1)
    Sys.sleep(2)
    rm(df1)}
  
  df3<-data.frame()
  for(i in 1:length(vlist[[length(vlist)]])){
    address <- 'https://www150.statcan.gc.ca/t1/wds/rest/getDataFromVectorsAndLatestNPeriods'
    #POST BODY
    payload <-data.frame(vectorId=vlist[[length(vlist)]][i], latestN=Obs$Freq[1])
    
    #Execute API query
    response<-POST(url=address, body=payload, encode="json")
    
    #Return query result
    res<-content(response)
    
    #Return to a dataframe
    framelist<-list()
    for(j in 1:as.integer(Obs$Freq[1])){
      frame<-data.frame(res[[1]]$object$vectorDataPoint[[j]]$refPer, res[[1]]$object$vectorDataPoint[[j]]$value, res[[1]]$object$vectorDataPoint[[j]]$frequencyCode, res[[1]]$object$vectorDataPoint[[j]]$scalarFactorCode)
      names(frame)<-c( "Date", "Value", "freq", "scalar")
      frame$freq<-freq[grepl(frame$freq, freq$code), "def"][1]
      frame$scalar<-scal[grepl(frame$scalar, scal$code), "def"][1]
      framelist[[j]]<-frame
    }
    df1<- do.call(rbind, framelist)
    
    df2<-SeriesInfo(vlist[[length(vlist)]][i], Observations)
    df1<-cbind(df1,df2)
    df3<-rbind(df3, df1)
    Sys.sleep(2)
    rm(df1)
  }
  
  df3<-cbind(aggregate(Value ~ Date, df3, sum), df3[1:Obs$Freq[1],3:length(df3)])
  df3$Series<-Vectors[grepl( vlist[[1]][1], Vectors$Vector), "Name.of.Group"]
  
  df<-rbind(df, df3)
  df$Cube<-CubeMetadata(res[[1]]$object$productId)
  return(df)
  rm(df2, df3)
}  

#Function to extract data from vectors for the latest N periods (unindexed)
datN.unindexed<-function(vlist,Observations){
  
  df<-data.frame()
  for(i in 1:(length(vlist)-1)){
    Obs<-SeriesInfo(vlist[[i]], Observations)
    
    address <- 'https://www150.statcan.gc.ca/t1/wds/rest/getDataFromVectorsAndLatestNPeriods'
    #POST BODY
    payload <-data.frame(vectorId=vlist[[i]], latestN=Obs$Freq[1])
    
    #Execute API query
    response<-POST(url=address, body=payload, encode="json")
    
    #Return query result
    res<-content(response)
    
    #Return to a dataframe
    framelist<-list()
    for(j in 1:as.integer(Obs$Freq[1])){
      frame<-data.frame(res[[1]]$object$vectorDataPoint[[j]]$refPer, res[[1]]$object$vectorDataPoint[[j]]$value, res[[1]]$object$vectorDataPoint[[j]]$frequencyCode, res[[1]]$object$vectorDataPoint[[j]]$scalarFactorCode)
      names(frame)<-c( "Date", "Value", "freq", "scalar")
      frame$freq<-freq[grepl(frame$freq, freq$code), "def"][1]
      frame$scalar<-scal[grepl(frame$scalar, scal$code), "def"][1]
      framelist[[j]]<-frame
    }
    df1<- do.call(rbind, framelist)
    
    df2<-SeriesInfo(vlist[[i]], Observations)
    df1<-cbind(df1,df2)
    df<-rbind(df, df1)
    Sys.sleep(1)
    rm(df1)}
  
  df3<-data.frame()
  for(i in 1:length(vlist[[length(vlist)]])){
    address <- 'https://www150.statcan.gc.ca/t1/wds/rest/getDataFromVectorsAndLatestNPeriods'
    #POST BODY
    payload <-data.frame(vectorId=vlist[[length(vlist)]][i], latestN=Obs$Freq[1])
    
    #Execute API query
    response<-POST(url=address, body=payload, encode="json")
    
    #Return query result
    res<-content(response)
    
    #Return to a dataframe
    framelist<-list()
    for(j in 1:as.integer(Obs$Freq[1])){
      frame<-data.frame(res[[1]]$object$vectorDataPoint[[j]]$refPer, res[[1]]$object$vectorDataPoint[[j]]$value, res[[1]]$object$vectorDataPoint[[j]]$frequencyCode, res[[1]]$object$vectorDataPoint[[j]]$scalarFactorCode)
      names(frame)<-c( "Date", "Value", "freq", "scalar")
      frame$freq<-freq[grepl(frame$freq, freq$code), "def"][1]
      frame$scalar<-scal[grepl(frame$scalar, scal$code), "def"][1]
      framelist[[j]]<-frame
    }
    df1<- do.call(rbind, framelist)
    
    df2<-SeriesInfo(vlist[[length(vlist)]][i], Observations)
    df1<-cbind(df1,df2)
    df3<-rbind(df3, df1)
    Sys.sleep(1)
    rm(df1)
  }
  
  df3<-cbind(aggregate(Value ~ Date, df3, mean), df3[1:Obs$Freq[1],3:length(df3)])
  df3$Series<-Vectors[grepl( vlist[[1]][1], Vectors$Vector), "Name.of.Group"]
  
  df<-rbind(df, df3)
  df$Cube<-CubeMetadata(res[[1]]$object$productId)
  return(df)
  rm(df2, df3)
}    

#Function to save metadata
Info.list<-list()
Title.list<-list()
png.list<-list()
bot.list<-list()
Meta<-function(abbr,vlist,Title,Info, df,pngname){
  #Save sentence
  Info.list[[i]]<<-Info
  
  #Save title
  Title.list[[i]]<<-Title
  
  #Create bottom text
  bot.list[[i]]<<-paste(df$Cube,". Vector IDs = v",paste(vlist[1:(length(vlist)-1)], collapse = ', v'),", v", paste(vlist[[length(vlist)]], collapse = ', v'), sep = "")[1]
  
  #Create a image name
  png.list[[i]]<<-pngname
}

#Function to print an indexed time series graph
TSInd<-function(vlist,Title, Observations,abbr){
  
  #Extract relevant data
  df<-datN(vlist,Observations[1])
  df$index<-NA
  Observations<-df$Freq[1]
  
  #Index
  for(i in 1:length(vlist)){
    df$index[(-(Observations-1)+Observations*i):(Observations*i)]<-df$Value[(-(Observations-1)+Observations*i):(Observations*i)]/df$Value[(-(Observations-1)+Observations*i)]*100
  }
  
  df$Date<-as.Date(df$Date)
  df$month<-months(df$Date)
  
  #Obtain variables for calculations
  m<-as.integer(strftime(df$Date,"%m"))[1]
  y<-as.integer(strftime(df$Date,"%Y"))[1]
  Sys.sleep(2)
  
  #Create a time series for the axis
  ts<-as.data.frame(df$Value[1:df$Freq[1]])
  ts<-ts(ts$`df$Value[1:df$Freq[1]]`, frequency = df$freq[1], start=c(y,m))
  
  #Date of latest observation
  if(df$freq[1] <=1){
    dt<-substr(tail(df$Date,1),1,4)
  }else if(df$freq[1] >=100){
    dt<-paste(df$month[Observations*length(vlist)]," ",substr(df$Date[Observations*length(vlist)],9,10), " ",substr(df$Date[Observations*length(vlist)],1,4), sep="")
  }else{dt<-paste(df$month[1],substr(tail(df$Date,1),1,4))}
  Sys.sleep(1)
  
  #Obtain latest observation
  dt1<-tail(df,1)$Value
  Sys.sleep(1)
  
  #Percentage of the latest information
  dt2<-percent(dt1/(df$Value))[Observations*(length(vlist)-1)]
  
  #Second to last category
  if(substring(df$Series[(length(vlist)-1)*Observations],1,5)=="Total"){
    dt3<-substring(tolower(df$Series), 7)[(length(vlist)-1)*Observations]
  }else{dt3<-tolower(df$Series)[(length(vlist)-1)*Observations]}
  
  #Write a descriptive sentence
  if(df$uomnum[1] %in% c(80:122, 284, 309, 359)){
    Info<-paste("In ",dt,", ",tolower(tail(df,1)$Series)," accounted for $",dt1,df$scalar,substring(tolower(tail(df,1)$uom),2)," of ",tolower(Title),", representing ",dt2," of the ",dt3, " ",tolower(Title),".", sep="")[1]
  }else if(df$uomnum[1] %in% 239:245){
    Info<-paste("In ",dt,", ",tolower(tail(df,1)$Series)," accounted for ",dt1," ", df$scalar,tolower(tail(df,1)$uom)," of ",tolower(Title),".", sep="")[1]
  }else if(df$uomnum[1] %in% c(123:128)){
    Info<-paste("In ",dt,", ",tolower(tail(df,1)$Series)," accounted for $",dt1,df$scalar,substring(tolower(tail(df,1)$uom),8)," of ",tolower(Title),", representing ",dt2," of the ",dt3, " ",tolower(Title),".", sep="")[1]
  }else if(df$uomnum[1]==75){
    Info<-paste("In ",dt,", ",tolower(tail(df,1)$Series)," accounted for $",dt1,df$scalar," of ",tolower(Title),", representing ",dt2," of the ",dt3, " ",tolower(Title),".", sep="")[1]
  }else{
    Info<-paste("In ",dt,", ",tolower(tail(df,1)$Series)," accounted for ",dt1, " ",df$scalar,tolower(tail(df,1)$uom)," of ",tolower(Title),", representing ",dt2," of the ",dt3," ",tolower(Title),".",sep="")[1] 
  }
  Sys.sleep
  
  #Name of .png
  pngname<-paste(Title,".png")
  
  #Create image
  png(pngname, unit="in", width = 9, height = 4, res=1200)
  print(ggplot(df, aes(x=Date, y=index, colour=Series, group=Series))+geom_path()+theme(axis.title = element_blank(), axis.title.y = element_blank(),plot.title = element_text(hjust = 0.5), legend.title = element_blank(), legend.position = "bottom") +ggtitle(paste(Title,"(indexed, 100 =",as.yearmon(as.data.frame(time(ts))[1,1]),")")))
  dev.off()
  
  #Save Metadata
  Meta(abbr, vlist, Title, Info, df, pngname)
}

#Function to print an non-indexed time series graph
TSUn<-function(vlist,Title, Observations, abbr){
  
  #Extract relevant data
  df<-datN.unindexed(vlist,Observations)
  Observations<-df$Freq[1]
  
  df$Date<-as.Date(df$Date)
  df$month<-months(df$Date)
  
  #Obtain variables for calculations
  m<-as.integer(strftime(df$Date,"%m"))[1]
  y<-as.integer(strftime(df$Date,"%Y"))[1]
  Sys.sleep(2)
  
  #Create a time series for the axis
  ts<-as.data.frame(df$Value[1:Observations])
  ts<-ts(ts$`df$Value[1:Observations]`, frequency = df$freq[1], start=c(y,m))
  
  
  #Date of latest observation
  if(df$freq[1] <=1){
    dt<-substr(tail(df$Date,1),1,4)
  }else if(df$freq[1] >=100){
    dt<-paste(df$month[Observations*length(vlist)],substr(df$Date[Observations*length(vlist)],9,10),substr(df$Date[Observations*length(vlist)],1,4))
  }else{dt<-paste(df$month[1],substr(tail(df$Date,1),1,4))}
  Sys.sleep
  
  #Obtain latest observation
  dt1<-tail(df,1)$Value
  Sys.sleep(2)
  
  #Percentage of the latest information
  dt2<-percent(dt1/(df$Value))[Observations*(length(vlist)-1)]
  
  #Second to last category
  if(substring(df$Series[(length(vlist)-1)*Observations],1,5)=="Total"){
    dt3<-substring(tolower(df$Series), 7)[(length(vlist)-1)*Observations]
  }else{dt3<-tolower(df$Series)[(length(vlist)-1)*Observations]}
  
  #Write a descriptive sentence
  if(df$uomnum[1] %in% c(80:122, 284, 309, 359)){
    Info<-paste("In ",dt,", ",tolower(tail(df,1)$Series)," accounted for $",dt1,df$scalar,substring(tolower(tail(df,1)$uom),2)," of ",tolower(Title),", representing ",dt2," of the ",dt3, " ",tolower(Title),".", sep="")[1]
  }else if(df$uomnum[1] %in% 239:245){
    Info<-paste("In ",dt,", ",tolower(tail(df,1)$Series)," accounted for ",dt1,df$scalar,tolower(tail(df,1)$uom)," of ",tolower(Title),".", sep="")[1]
  }else if(df$uomnum[1] %in% c(123:128)){
    Info<-paste("In ",dt,", ",tolower(tail(df,1)$Series)," accounted for $",dt1,df$scalar,substring(tolower(tail(df,1)$uom),8)," of ",tolower(Title),", representing ",dt2," of the ",dt3, " ",tolower(Title),".", sep="")[1]
  }else if(df$uomnum[1]==75){
    Info<-paste("In ",dt,", ",tolower(tail(df,1)$Series)," accounted for $",dt1,df$scalar," of ",tolower(Title),", representing ",dt2," of the ",dt3, " ",tolower(Title),".", sep="")[1]
  }else{
    Info<-paste("In ",dt,", ",tolower(tail(df,1)$Series)," accounted for ",dt1,df$scalar,tolower(tail(df,1)$uom)," of ",tolower(Title),", representing ",dt2," of the ",dt3,tolower(Title),".",sep="")[1] 
  }
  Sys.sleep
  
  #Name of .png
  pngname<-paste(Title,".png")
  
  #Create image
  png(pngname, unit="in", width = 9, height = 4, res=1200)
  print(ggplot(df, aes(x=Date, y=Value, colour=Series, group=Series))+geom_path()+theme(axis.title.x = element_blank(),plot.title = element_text(hjust = 0.5), legend.title = element_blank(), legend.position = "bottom") +ggtitle(Title)+ylab(tolower(tail(df,1)$uom)))
  dev.off()
  
  #Save Metadata
  Meta(abbr, vlist, Title, Info, df, pngname)
}

####Slide maker ####
for(i in 1:length(unique(Vectors$Indicator))){
  vlist<-list()
  FdBev<-Vectors[(Vectors$Indicator==unique(Vectors$Indicator)[i]& Vectors$Group=="Y"), 3]
  toplev<-Vectors[(Vectors$Indicator==unique(Vectors$Indicator)[i]& Vectors$Group=="N"), 3]
  for(j in 1:length(toplev)){
    vlist[[j]]<-toplev[j]
  }
  vlist[[length(toplev)+1]]<-FdBev
  
  #Input the title
  Title<-as.character(unique(Vectors$Indicator)[i])
  
  #Input the desired number of observations
  Observations<-subset(Vectors, Vectors$Indicator==as.character(unique(Vectors$Indicator)[i]))$Number.of.Years[1]
  
  #Input a generic abbreviation for this slide
  abbr<-as.character(unique(Vectors$Indicator)[i])
  
  ifelse(Vectors[grepl(as.character(unique(Vectors$Indicator)[i]), Vectors$Indicator), 4][1]=="Indexed", 
         TSInd(vlist, Title, Observations, abbr), TSUn(vlist, Title, Observations, abbr))
  
}

####Deck####

#Make the Deck
my_pres<- read_pptx("S:\\SI Life Sciences\\13733-Economic Analysis_Pharma\\Data\\R Extract Project\\Charts and Templates\\ISED_BrandOverview-EN.pptx") %>% 
  #Title Slide
  add_slide(layout="Title", master="Office Theme") %>%
  ph_with_text(type="title", str=Vectors$Deck.Title[1]) %>% #Title input
  ph_with_text(type="subTitle", str=paste("Last Update:", format(Sys.Date(),"%d/%m/%Y"))) %>%
  #Slides
  add_slide(layout="Custom Layout", master="Office Theme") %>%
  ph_with_img(index=3, src=png.list[[1]]) %>%
  ph_with_text(type="title", str=Title.list[[1]])%>%
  ph_with_text(type="body", index=4, str=Info.list[[1]]) %>%
  ph_with_text(type="body", index=2, str=bot.list[[1]])%>%
  #Export
  print(target="Trends.pptx") %>%
  invisible()

if(length(unique(Vectors$Indicator)) >=2){
  
  for(i in 2:length(unique(Vectors$Indicator))){
    my_pres<- read_pptx("Trends.pptx") %>% 
      #Slides
      add_slide(layout="Custom Layout", master="Office Theme") %>%
      ph_with_img(index=3, src=png.list[[i]]) %>%
      ph_with_text(type="title", str=Title.list[[i]])%>%
      ph_with_text(type="body", index=4, str=Info.list[[i]]) %>%
      ph_with_text(type="body", index=2, str=bot.list[[i]])%>%
      #Export
      print(target="Trends.pptx") %>%
      invisible()
  }
}

my_pres<- read_pptx("Trends.pptx") %>% 
  #Slides
  add_slide(layout="2_Comparison", master="Office Theme") %>%
  #Export
  print(target="Trends.pptx") %>%
  invisible()


end_time <- Sys.time()
end_time - start_time
