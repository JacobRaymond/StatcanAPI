#Load some packages
pacman::p_load(httr)
  
  
  #Suppose we want to know the probation rate per 10,000 young persons for Newfoundland and Labrador, found in table 35100003.
  
  #The first step is to find the corresponding vector. The only way to get it for now (checked with Stats Can) is to download the entire table in csv format and look manually. This isn't optimal but it's necessary.
  #To do this, use the getFullTableDownloadCSV method.
  
  pid<-"27100333" #Enter table number (In this case 35100003)
  
  #This method basically returns the url to download the full table (in a zipped folder). These next lines of code download the folder (as a temporary file, this doesn't use memory), unzip the folder, retrieve the table and import it. Don't worry about the details, it's the same format for all tables.
  {
    #Prep url address
    urltext<-paste("https://www150.statcan.gc.ca/t1/wds/rest/getFullTableDownloadCSV/",pid,"/en")
    urltext<-gsub(" ","",urltext) 
    
    #This is a get method, so you don't need a body.
    response<-GET(url=urltext, encode="json")
    df<-content(response)
    
    #Download
    temp<-tempfile()
    download.file(df[[2]][1], temp)
    
    #Unzip
    filename=paste(pid,".csv")
    filename<-gsub(" ","",filename) 
    df<-read.csv(unz(temp,filename))
    unlink(temp)
    
    View(df)
    
  }
  
  
  #From there, we can see the vector for the probation rate per 10,000 young persons for Newfoundland and Labrador is v32164132.
  #Now, suppose we want to know this rate for the last 5 periods. We can use the getDataFromVectorsAndLatestNPeriods method:
  
  {
    #Prep the address
    urltext <- 'https://www150.statcan.gc.ca/t1/wds/rest/getDataFromVectorsAndLatestNPeriods'
    
    #Input the body according to the following format. Our vectorId is 32164132 (remove the v) and we want the latest 5 periods.
    payload <-data.frame(vectorId="32164132", latestN="5")
    
    #Execute
    res<-POST(url=urltext, body=payload, encode="json")
    res<-content(res)
    
    #We now have to extract our data. At this point, "response" is a list containing multiple lists. We have to do some digging (click on "response" and navigate through the lists. The format should be consistent, but there could be some differences)
    
    #Finally, using a loop, we can add everything to a dataframe:
    framelist<-list()
    for(i in 1:5){ #Continue the loop for the necessary number of observations
      
      #Pulls out the specific observation corresponding to the period
      frame<-data.frame(res[[1]]$object$vectorDataPoint[[i]]$refPer, res[[1]]$object$vectorDataPoint[[i]]$value)
      names(frame)<-c("Date", "Value")
      framelist[[i]]<-frame
    }
    #Finally, create the dataframe
    ProbationNL<-do.call(rbind, framelist)
  }
  