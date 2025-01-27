

#' Phenologic metrics from time series vegetation index data
#' 
#' @return  Onset - the NDVI value at the start of the greenness
#' @return  OnsetT - the time when the greeness of crop starts
#' @return  MaxV - the annual maximum NDVI during Anthesis stage of crop
#' @return  MaxT - the time when the maximum NDVI occurs
#' @return  Offset - the NDVI value at the point before senesence stage of the crop 
#' @return  OffsetT - the time when the offset occured
#' @return  GreenUpSlope- the rate of increase in NDVI between onset and Maximum NDVI
#' @return  BrownDownSlope - The rate of decrease in NDVI from maximum NDVI to sensence
#' @return  LengthGS- the length of the growing season between Onset and Offset
#' @return  AreaBeforeMax - the integral area under the curve between Onset and Maximum NDVI
#' @return  AreaAfterMax - the integral area under the curve between Maximum NDVI and Offser
#' @return  TINDVI - the integral area under the curve 
#' @return  Asymmetry - the difference between AreaBeforeMax and AreaAfterMax 
#' @author   Sofanit Araya
#' @keywords Phenology, remote sensing, satellite image
#' @seealso MultiPointsPlot (N,Id1, Id2...Idn)
#' @description This function extracts major phenologic parameters from Moderate Resolution Imaging Spectroradiometer (MODIS)  time series vegetaion index data. Total of 11 phenologic metrics as raster and Ascii files. The function takes path of the vegetation index data and the boolean Value for BolAOI (True- if there is AOI polygon, FALSE- if the parameters are calculated for the whole region).
#' @param Rawpath - Text value - the path where the time series images saved 
#' @param BolAOI-  Logical value - if there is any area of intererst or not
#' @export
#' @details
#'Remote sensing phenology also called Land Surface Phenology refers to observation of seasonal pattern of vegetation changes using remote sensing vegetation indices (Reed etal. 2009).
#'Remotely sensed vegetation phenology has been used for many ecological studies including as an indicator for climate change (Kramer et al.,2000; White et al., 1997), to estimate agricultural productivity (Hill and Donald, 2003; Labus et al., 2002; Sakamoto et al., 2013), regional management for crop type mapping (Brown et al., 2013; Niazmardi et al., 2013), as an indicator of soil Plant Available Water Holding Capacity (PAWC) variability across a farm (Araya etal. 2015)  and many more applications. 
#'Different methods have been employed to extract phenologic metrics, which include threshold definition (White et al., 1997), decomposition of the vegetation dynamic curve using harmonic analysis (Jakubauskas et al., 2001; Roerink et al., 2011) (Zhang et al., 2003) , taking the first derivative of the smoothed and non-smoothed vegetation index dynamics curves (Moulin et al., 1997) and defining the crossover point of the smoothed and non-smoothed dynamics curves (Hill and Donald, 2003; Reed et al., 1994). In this package the phenologic metrics were extracted based on correlation of the description of crop physiological stages (Zadoks etal. 1974) with the relative greenness of the crop on the vegetation dynamics. 
#'The list of the phenologic metrics and their description are provided at the website - www.cropphenology.wix.com/package
#'
#'Detail of metrics definition
#'
#'--------------
#'
#'OnsetT and OnsetV
#'
#'
#'The OnsetT and OnsetV are defined as the value and time when the crop starts attaining high vegetation index increasingly. 
#'
#'Thechnically, the algorthm used looks like as follows:
#' 
#' 
#'ALGORITHM
#'
#'  
#' Assumption - 
#' 
#'1-Farm clearance done during time of  image 5 and image 6 so the trushold is considered to be mean of image 5 and image 6
#' 
#' trsh= (image5+image6)/2
#' 
#'2- The time frame for onset is between MODIS imaging time 7 and 12. We calculate the slope of the connecting line between values of images in that period
#' 
#' Slope=array of  (slope b/n 7 and 8, slope b/n 8 and 9, slope b/n 9 and 10, slope b/n 10 and 11, slope b/n 11 and 12
#' 
#'3- The slope observed next to trough is considered as Onset. So we calculate the last slope bellow - 0.01, just to avoid the minor details
#'                  
#'The last negative slope between image 7 and image 12
#'
#'Case 1 - No negative slope (i.e all increasing)
#'  
#'  Step 1 - compare the values with the trsh value and the point where the trsh is excedded is Onset
#'  
#'     Case 1.1 - check if the next slope is -ve , although it is above -0.01
#'     
#'        If yes => take the next 
#'        
#'        If no => the point where the trsh is exceeded is Onset
#'        
#'     Case 1.2- if trsh is very high (sometimes due to uncleared weed or any other causes…)
#'     
#'                  Take the highest slope
#'                  
#'Case 2- last -ve is at the end of the onset time frame
#'
#'  Check the previous slope (does it come from down or just a small trough?)
#'    
#'    Case 2.1 Came from down (previous is -ve)
#'
#'              Check if the slope increases at image 12 then Onset is at 12
#'
#'    Case 2.2 previous slope is +ve (it is a small trough)
#'    
#'              Ignore and consider it as all positive and take the point where threshold exceeded
#'Case3 - last -ve between 2 and 5
#'    
#'    Check previous slope
#'    
#'    Case 3.1 - if previous slope is +ve
#'          
#'          Consider it as a small trough and take the threshold exceeding point as Onset
#'          
#'    Case 3.2 - previous slope is -ve
#'
#'         The onset is that point where the +ve slope started 
#'
#'Case 4 - last -ve at the  beginning (i.e slope 1 or 2)
#'
#'        Consider that as low vegetation after clearance and take the point where trsh exceeded as Onset
#'
#'--------------------
#'
#' OffsetV and OffsetT
#' 
#'  
#' OffsetV and OffsetT are defined as the vegetation index value and time when the plant senesence. On the timeseries vegetation curve, OffsetV and OffsetT are defined in the following few steps
#' 
#' ALGORITHM
#' 
#' 1st- Calculate the offset thrushold value (trsh2)
#' The threshold is defined as 10% above the total non green reference at the end pf the growing season, i.e minimum of the 22 and 23 MODIS imaging periods.
#' 2nd- the slope of the line between consicutive values between 19th and 23rd image values are calculated.These values are -ve.
#' 3rd- calculate the minimum of the absolute values of the slope values.
#' 4th- starting from the minimum curve onward check the value for below trsh2
#' 5th- In some cases the consicutive +ve slopes indicate the big break (offset), so check if there is posetive slope after 20th image if so that will be considered Offset
#' 
#' 
#' The threshold is defined as 10% above the total non green reference at the end pf the growing season, i.e average of 22 and 23 MODIS imaging periods.
#' 
#' --------------
#' 
#' MaxV and MaxT 
#' 
#' 
#' MaxV and MaxT are defined as he value and time when the maximum vegetation index value attained during the growing season.
#' 
#'  Maximum (NDVI1, NDVI23)
#' 
#' 
#'@examples 
#' # EXAMPLE - 1
#'
#' # phenologic metrics for region at the western Eyre Peninsula, South Australia. 
#' # The Raster images are clipped to the AOI, AOI = FALSE

#' PhenoMetrics(system.file("extdata/data1", package="CropPhenology"), FALSE)
#' 
#' 
#' # EXAMPLE - 2
#' # Phenologic metrics for region at the Eastern Eyre Peninsula, South Australia. 
#' # Polygon boundary used as AOI from the MODIS images thus AOI= TRUE
#' 
#' PhenoMetrics(system.file("extdata/data2", package="CropPhenology"), TRUE)
#' 
#' # In both examples the result will be saved at the working directory under the folder "Results"
#' 
#' @references Araya, S., Lyle, G., Lewis, M., Ostendorf, B., in press. Phenologic metrics derived from MODIS NDVI as indicators for Plant Available Water-holding Capacity. Ecol. Ind.
#' @references Brown, J.C., Kastens, J.H., Coutinho, A.C., Victoria, D.d.C., Bishop, C.R., 2013. Classifying multiyear agricultural land use data from Mato Grosso using time-series MODIS vegetation index data. Rem. Sens. of Env/ 130, 39-50.
#' @references Hill, M.J., Donald, G.E., 2003. Estimating spatio-temporal patterns of agricultural productivity in fragmented landscapes using AVHRR NDVI time series. Rem.Sen. Env. 84, 367-384.
#' @references Moulin, S., Kergoat, L., Viovy, N., Dedieu, G., 1997. Global-scale assessment of vegetation phenology using NOAA/AVHRR satellite measurements. Journal of Climate 10, 1154-1170.
#' @references Reed, B.C., Schwartz, M.D., Xiao, X., 2009. Remote Sensing Phenology: Status and the Way Forward, in: Noormets, A. (Ed.), Phenology of Ecosystem Processes. Springer New York, pp. 231-246.
#' @references Roerink, G.J., Danes, M.H.G.I., Prieto, O.G., De Wit, A.J.W., Van Vliet, A.J.H., 2011. Deriving plant phenology from remote sensing, 2011 6th Int. Wor. on the, pp. 261-264.
#' @references Sakamoto, T., Gitelson, A.A., Arkebauer, T.J., 2013. MODIS-based corn grain yield estimation model incorporating crop phenology information. Rem. Sen.of Env. 131, 215-231.
#' @references White, M.A., Thornton, P.E., Running, S.W., 1997. A continental phenology model for monitoring vegetation responses to interannual climatic variability. Glo. Biogeochem. Cyc. 11, 217-234.
#' @references Zadoks, J.C., Chang, T.T., Konzak, C.F., 1974. A decimal code for the growth stages of cereals. Weed Research 14, 415-421.

PhenoMetrics<- function (RawPath, BolAOI){
  

  require('shapefiles')
  require("raster")
  require("maptools")
  require("rgdal")
  require("xlsx")
  require("rgeos")
  require("grid")
  
  setwd(RawPath)
  raDir=dir(path=RawPath, pattern = c(".img$|.tif$"))
  FileLen=length(raDir)
  q=1
  qon=1
  qoff=1
  qmax=1
  par(mfrow=c(1,1))
  par(mar=c(3.5, 2.5, 2.5, 5.5))
  s=1
  if (BolAOI == TRUE){
    BolAOI=dir(pattern="*.shp$")
    shp=readShapePoly(BolAOI)
  }
  
  if (BolAOI == FALSE){
    ra=raster(raDir[1])    
    Points=rasterToPoints(ra)
    shp=rasterToPolygons((ra*0), dissolve=TRUE)
  }
  
  
  i=1
  try=0
  
  if (FileLen==0){ stop ('No image file obtained in the path mensioned - Check your file type')}
  if (FileLen<23){ stop ('The number of images not complete cover the season - check your image files')}
  
  while (i<(FileLen+1)) {
    ras=raster(raDir[i])
    try[i]=extract (ras,shp, cellnumbers=TRUE)
    i=i+1
  }
  
  try
  
  
  cor=xyFromCell(ras,try[[1]][,"cell"])
  com=cbind(cor,try[[1]])
  
  Onset_Value=com
  Onset_Time=com
  Offset_Value=com
  Offset_Time=com
  Max_Value=com
  Max_Time=com
  Area_Total=com
  Area_Before=com
  Area_After=com
  Asymmetry=com
  GreenUpSlope=com
  BrownDownSlope=com
  LengthGS=com
  BeforeMaxT=com
  AfterMaxT=com
  Amplitude=com
  
  
  r=length(try[[1]][,"value"])
  Hd=list ("X-Cord"," Y_Cord","T1", "T2", "T3" ,"T4", "T5", "T6", "T7", "T8", "T9", "T10", "T11", "T12", "T13", "T14", "T15", "T16", "T17", "T18", "T19", "T20", "T21", "T22", "T23")  
  AllP=data.frame()
  while(s>0 & s<(r+1)){ #iterate through the each pixel
    
    #+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
    # Plot the time series curve of the year for the sth pixel
    #+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
    
    AnnualTS=as.matrix(0)
    q=1 #reset qon for the next pixel comparision
    #===================== Iterate throught the files for s-th pixels to get the curve
    
    while (q>0 & q<FileLen+1){
      GRD_CD=(try[[q]][,"value"][s])/10000
      if (is.na(GRD_CD)){
        GRD_CD=(try[[q-1]][,"value"][s])/10000
      }
      AnnualTS[q]=GRD_CD
      q=q+1
    }
    
    #print (AnnualTS)
    cordinate=0
    cordinate[1]=cor[s,1]
    cordinate[2]=cor[s,2]
    AP=append(cordinate,AnnualTS)
    AllP=rbind(AllP, AP)
#    ts.plot(AnnualTS)
    
    #+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
    #                                                  Maximum
    #+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
    qmax=1
    max=AnnualTS[qmax]
    Max_T=qmax
    
    while (qmax>0 & qmax<FileLen){
      if ((AnnualTS[qmax]) > max){
        max=AnnualTS[qmax]
        #print (max)
        Max_T=qmax
      }
      qmax=qmax+1
    }
    Max_Value[,"value"][s]=max
    Max_Time[,"value"][s]=Max_T    
    
    
    #+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
    #                                                  Onset 
    #+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
    
    # successive slops b/n  points
    j=7
    slop=(AnnualTS[j+1]-AnnualTS[j])
    slop=as.matrix(slop)
    f=1
    while (j<12){
      slop[f]=(AnnualTS[j+1]-AnnualTS[j])
      j=j+1
      f=f+1
    }
    min1=mean(AnnualTS[5:6]) #minimum before the amplitude
    min2=mean(AnnualTS[22:23]) #minimum after the amplitude
    range1=0.1*min1 #to get 20% of the min before Max
    range2=0.1*min2#to get 20% of the min after Max
    trsh1=min(AnnualTS[5:6])+range1 # to get 20% more greenness than the min before max
    trsh2=min(AnnualTS[22:23])+range2 # to get 20% more greenness than the min after max
    #------------------------------------------------------------------------------------------------------------------------------
    # last -ve slop
    len=length(slop)
    last=slop[len]
    i=len
    ls=0
    while(i<len+1 & i>1){
      if (last<0){
        #print(last)
        if (last< (-0.01)){
          #print(last)
          ls=i
          break
        }
        quick=i
      }
      i=i-1
      last=slop[i]
    }
    
    #print (ls)
    #------------------------------------------------------------------------------------------------------------------------------
    #maximum of the post negetive records
    R_max=0
    max_r=0
    Em=0
    k=7 # to check the early point where the trushold reached
    if (ls==0){ #for all +ve slop
      touched=FALSE
      while (k<12){
        if (AnnualTS[k]>trsh1){
          #then check for trsh1
          if (slop[k-6]>=0){
            Em=k
            touched=TRUE
            break
          }
          if (slop[k-6]<0){
            Em=k+1
            touched=TRUE
            break
            
          }
          touched=TRUE
        }
        
        k=k+1
      }
      if (touched==FALSE){
        c=1
        while(c<6){
          if (slop[c]== max(slop)){
            Em=c+6
          }
          c=c+1
        }
      }
    }
    # ls=5 =>  the last slope (i.e b/n 11 and 12 is decreasing), 
    #in this case check for the previous and next slope if it is not continue decreasing but was decreasing before that then 
    # the onset is at image 12 
    
    if (ls==5){
      if ((slop[ls-1]<0) & ((AnnualTS[13]-AnnualTS[12])>0)){
        Em=12
      }
      if ((slop[ls-1]<0) & ((AnnualTS[13]-AnnualTS[12])<0)){
        if (AnnualTS[10]>trsh1){Em=10 }
        if (AnnualTS[10]<trsh1){Em=13}
      }
      
      if (slop[ls-1]>0){
        k=7
        while (k<12){
          if ((AnnualTS[k]>trsh1) & (slop[k-6]>0)){
            Em=k
            break
          }
          k=k+1
        }
      }
      if (Em==0){ Em=12} # if incase the posetive slop in before 11 like -,-,-,+,- slope
    }
    
    
    if ((ls<5)& (ls>2)){
      
      if (slop[ls-1]<(-0.01)){ # if the previous is -ve, i.e deep decreament
        Em=ls+7
      }
      touched=FALSE
      if (slop[ls-1]>(-0.01)){ #if it is minor decrease on genneral +ve trend
        k=7
        while (k<12){
          if (AnnualTS[k]>trsh1){ # check pt where the trushold passed
            if (slop[k-6]>=0){
              Em=k
              touched=TRUE
              break
            }
            if (slop[k-6]<0){
              Em=k+1
              touched=TRUE
              break
            }
            touched=TRUE
          }
          k=k+1
        }
        if (touched==FALSE){
          Em=ls+6
        }
      }
    }
    if((ls==1) | (ls==2)){ #if the -ve slope is at the start
      k=ls+7
      tk=1
      while (k<12){
        if (AnnualTS[k]>trsh1){ # check the point where trsh passed
          g=k-6
          if (slop[k-6]>(-0.01)){ # check if the next slope is negetive- if posetive take that as onset if not keep checking
            Em=k
            tk=tk+1
            break
          }
        }
        k=k+1
      }
      if (tk==1){ Em=12 } # if the slops gets all -ve after ls -  the last point will be taken as Onset
    }
    #------------------------------------------------------------------------------------------------------------------------------
    t=((AnnualTS[Em]-trsh1)/(AnnualTS[Em]-AnnualTS[Em-1]))
    if (t==0){
      Onset=Em  
    }
    if (((t>0) & (AnnualTS[Em]-AnnualTS[Em-1])>0 &(Em>t))){
      onset=Em-t
      onsetV= ((AnnualTS[Em]-AnnualTS[Em-1])*(1-t))+AnnualTS[Em-1]
    }
    
    if ((t<0) | (AnnualTS[Em]-AnnualTS[Em-1])<0){
      onset=Em
      onsetV=AnnualTS[Em]
    }
    if (Em>t){
      onset=Em
    }
    #print (Em)
    #print (trsh1)
    
  
    #==============================
    
    #print (Em)
    #print (trsh1)
    onset=Em
    onsetV=AnnualTS[Em]
    
    print (Max_T)
    
    if (onset > (Max_T)){
      onset=7
    }
    
    Onset_Value[,"value"][s]=onsetV
    Onset_Time[,"value"][s]=onset
    #print (s)
    #print (r)
    #print (onset)
    #print(onsetV)

    #+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
    #                                                  Offset
    #+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
    tm=min(AnnualTS[22], AnnualTS[23])
    trsh2=(tm+(tm/10))
    ofslp=AnnualTS[19]-AnnualTS[18]    
    crp=TRUE
    ofslp=matrix(ofslp)
    ofslp[2]=AnnualTS[20]-AnnualTS[19]
    ofslp[3]=AnnualTS[21]-AnnualTS[20]
    ofslp[4]=AnnualTS[22]-AnnualTS[21]
    ofslp[5]=AnnualTS[23]-AnnualTS[22]
    
    minof=abs(ofslp[1])
    
    ofc=1
    oft=18
    
    while (ofc<5) {
      if (minof>abs(ofslp[ofc])){
        minof=ofslp[ofc]
        oft=ofc+17
      }
      ofc=ofc+1
    }
    
    i=oft
    while (i<23){
      if ((AnnualTS[i]<trsh2)){
        offsetT=i
        offsetV=AnnualTS[i]
        break
      }
      i=i+1
    }
    
    #print (offsetT)
    #print (offsetV)
    #print(trsh2)
    
    
    if ((max-trsh2)<0.05) {
      crp=FALSE
      offsetT=0
      offsetV=0
    }
    if (offset>20){
      if (ofslp[3]>0){
        OffsetT=22
        offsetV=AnnualTS[22]
        
      }
      if (ofslp[2]>0){
        OffsetT=21
        offsetV=AnnualTS[21]
        
      }
      if (ofslp[1]>0){
        OffsetT=20
        offsetV=AnnualTS[20]
      }
      
    }
    
    
    #print (offsetT)
    #print (offsetV)
    
    Offset_Value[,"value"][s]=offsetV
    Offset_Time[,"value"][s]= offsetT
    
  
    #+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
    #                                                Area
    #+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
    
    
    St=abs(round (onset))
    Ed=abs(round(offset))
    Area=0
    start=St
    end=Ed+1
    mx=Max_T
    
    if (St<=0) {
      start=9
      start1=9
      start2=9
    }
    
    if (crp==FALSE){
      Area=0
      Area1=0
      Area2=0
    }
    while (start<end){
      Area=Area+AnnualTS[start]
      start=start+1
    }
    #print (Area)
  
    Area_Total[,"value"][s]=Area
    
    
    start1=St
    Area1=AnnualTS[start1]/2
    start1=start1+1
    while (start1<mx){
      Area1=Area1+AnnualTS[start1]
      start1=start1+1
    }
    Area1=Area1+AnnualTS[mx]/2
    #print (Area1)
    if (Area==0){ Area1=0}
    Area_Before[,"value"][s]=Area1
    
    Area2=AnnualTS[mx]/2
    start2=mx+1
    while (start2<(end)){
      Area2=Area2+AnnualTS[start2]
      start2=start2+1
    }
    Area2=Area2+AnnualTS[Ed]/2
    #print (Area2)
    if (Area==0){ Area2=0}
    Area_After[,"value"][s]=Area2
    
    Asy=Area1-Area2
    Asymmetry[,"value"][s]=Asy
    
    s=s+1
    
  }
  
  dir.create("Metrics")
  setwd(paste(getwd(), "Metrics", sep="/"))
  
  
  write.table(Area_Total, "TINDVI.txt")
  write.table(Area_After, "TINDVIAfterMax.txt")
  write.table(Area_Before, "TINDVIBeforeMax.txt")
  
  write.table(Max_Value, "Max_V.txt")
  write.table(Max_Time, "Max_T.txt")
  
  write.table(Offset_Value, "Offset_V.txt")
  write.table(Offset_Time, "Offset_T.txt")
  
  write.table(Onset_Value, "Onset_V.txt")
  write.table(Onset_Time, "Onset_T.txt")
  
  write.table(Asymmetry, "Asymmetry.txt")
  ###===================================================================================================
  #Defining secondary metrics
  
  BeforeMaxT[,"value"]=Max_Time[,"value"]-Onset_Time[,"value"]
  write.table(BeforeMaxT, "BeforeMaxT.txt")
  
  AfterMaxT[,"value"]=Offset_Time[,"value"]-Max_Time[,"value"]
  write.table(AfterMaxT, "AfterMaxT.txt")
  
  #BrownDownSlope=Max_Time
  BrownDownSlope[,"value"]=(Max_Value[,"value"]-Offset_Value[,"value"])/(Offset_Time[,"value"]-Max_Time[,"value"])
  write.table(BrownDownSlope, "BrownDownSlope.txt")
  
  #GreenUpSlope=Max_Time
  GreenUpSlope[,"value"]=(Max_Value[,"value"]-Onset_Value[,"value"])/(Max_Time[,"value"]-Onset_Time[,"value"])
  write.table(GreenUpSlope, "GreenUpSlope.txt")
  
  
  #LengthGS=Max_Time
  LengthGS[,"value"]=(Offset_Time[,"value"]-Onset_Time[,"value"])
  write.table(LengthGS, "LengthGS.txt")
  
  #LengthGS=Max_Time
  Amplitude[,"value"]=(Max_Value[,"value"]-((Onset_Value[,"value"]+Offset_Value[,"value"])/2))
  write.table(Amplitude, "Amplitude.txt")
  
  ###===================================================================================================
  par(mfrow=c(2,2))
  
  names(AllP)=Hd
  write.csv(AllP, "AllPixels.txt")
  
  
  OT=rasterFromXYZ(Onset_Time)
  crs(OT)<-crs(ras)
  brk=seq(6,13, by=0.01)
  nbrk=length(brk)
  plot(OT$value, main="OnsetT", breaks=brk, col=rev(terrain.colors(nbrk)), axis.arg=list(at=seq(6,13,by=2), labels=seq(6,13,by=2)), zlim=c(6,13))
  writeRaster(OT$value, "OnsetT.img", overwrite=TRUE)
  
  OV=rasterFromXYZ(Onset_Value)
  brk=seq(0.1,0.6, by=0.001)
  nbrk=length(brk)
  crs(OV)<-crs(ras)
  plot(OV$value, main="OnsetV", breaks=brk, col=rev(terrain.colors(nbrk)), axis.arg=list(at=seq(0.1,0.6,by=0.2), labels=seq(0.1,0.6,by=0.2)), zlim=c(0.1,0.6))
  writeRaster(OV$value, "OnsetV.img", overwrite=TRUE)
  
  MT=rasterFromXYZ(Max_Time)
  crs(MT)<-crs(ras)
  brk=seq(8,19, by=1)
  nbrk=length(brk)
  lblbrk=brk
  plot(MT$value, main="MaxT", breaks=brk, col=rev(terrain.colors(nbrk)), axis.arg=list(at=seq(8,19,by=2), labels=seq(8,19,by=2)),zlim=c(8,19))
  writeRaster(MT$value, "MaxT.img", overwrite=TRUE)
  
  
  MV=rasterFromXYZ(Max_Value)
  crs(MV)<-crs(ras)
  brk=seq(0.2,1, by=0.001)
  nbrk=length(brk)
  plot(MV$value, main="MaxV", breaks=brk, col=rev(terrain.colors(nbrk)), axis.arg=list(at=seq(0.2,1,by=0.2), labels=seq(0.2,1,by=0.2)), zlim=c(0.2,1))
  writeRaster(MV$value, "MaxV.img", overwrite=TRUE)
  
  OFT=rasterFromXYZ(Offset_Time)
  crs(OFT)<-crs(ras)
  brk=seq(16,23, by=0.01)
  nbrk=length(brk)
  plot(OFT$value, main="OffsetT", breaks=brk, col=rev(terrain.colors(nbrk)), axis.arg=list(at=seq(16,23,by=2), labels=seq(16,23,by=2)), zlim=c(16,23))
  writeRaster(OFT$value, "OffsetT.img", overwrite=TRUE)
  
  OFV=rasterFromXYZ(Offset_Value)
  crs(OFV)<-crs(ras)
  brk=seq(0,0.4, by=0.001)
  nbrk=length(brk)
  plot(OFV$value, main="OffsetV", breaks=brk, col=rev(terrain.colors(nbrk)), axis.arg=list(at=seq(0,0.4,by=0.1), labels=seq(0,0.4,by=0.1)), zlim=c(0,0.4))
  writeRaster(OFV$value, "OffsetV.img", overwrite=TRUE)
  
  GUS=rasterFromXYZ(GreenUpSlope)
  crs(GUS)<-crs(ras)
  brk=seq(0,0.25, by=0.00001)
  nbrk=length(brk)
  plot(GUS$value, main="GreenUpSlope", breaks=brk, col=rev(terrain.colors(nbrk)), axis.arg=list(at=seq(0,0.25,by=0.1), labels=seq(0,0.25,by=0.1)),zlim=c(0,0.25))
  writeRaster(GUS$value, "GreenUpSlope.img", overwrite=TRUE)
  
  BDS=rasterFromXYZ(BrownDownSlope)
  crs(BDS)<-crs(ras)
  brk=seq(0,0.25, by=0.00001)
  nbrk=length(brk)
  plot(BDS$value, main="BrownDownSlope", breaks=brk, col=rev(terrain.colors(nbrk)), axis.arg=list(at=seq(0,0.25,by=0.1), labels=seq(0,0.25,by=0.1)),zlim=c(0,0.25))
  writeRaster(BDS$value, "BrownDownSlope.img", overwrite=TRUE)
  
  BefMaxT=rasterFromXYZ(BeforeMaxT)
  crs(BefMaxT)<-crs(ras)
  brk=seq(0,12, by=0.01)
  nbrk=length(brk)
  plot(BefMaxT$value, main="BeforeMaxT", breaks=brk, col=rev(terrain.colors(nbrk)), axis.arg=list(at=seq(0,12,by=2), labels=seq(0,12,by=2)), zlim=c(0,12))
  writeRaster(BefMaxT$value, "BeforeMaxT.img", overwrite=TRUE)
  
  AftMaxT=rasterFromXYZ(AfterMaxT)
  crs(AftMaxT)<-crs(ras)
  brk=seq(0,12, by=0.01)
  nbrk=length(brk)
  plot(AftMaxT$value, main="AfterMaxT", breaks=brk, col=rev(terrain.colors(nbrk)), axis.arg=list(at=seq(0,12,by=2), labels=seq(0,12,by=2)), zlim=c(0,12))
  writeRaster(AftMaxT$value, "AfterMaxT.img", overwrite=TRUE)
  
  Len=rasterFromXYZ(LengthGS)
  crs(Len)<-crs(ras)
  brk=seq(6,17, by=0.1)
  nbrk=length(brk)
  writeRaster(Len$value, "LengthGS.img", overwrite=TRUE)
  plot(Len$value, main="LengthGS", breaks=brk, col=rev(terrain.colors(nbrk)), axis.arg=list(at=seq(6,17,by=2), labels=seq(6,17,by=2)), zlim=c(6,17))
  
  AA=rasterFromXYZ(Area_After)
  crs(AA)<-crs(ras)
  brk=seq(0,6, by=0.0001)
  nbrk=length(brk)
  plot(AA$value, main="TINDVIAfterMax", breaks=brk, col=rev(terrain.colors(nbrk)), axis.arg=list(at=seq(0,6,by=2), labels=seq(0,6,by=2)), zlim=c(0,6))
  writeRaster(AA$value, "TINDVIAfterMax.img", overwrite=TRUE)
  
  AB=rasterFromXYZ(Area_Before)
  crs(AB)<-crs(ras)
  brk=seq(0,6, by=0.0001)
  nbrk=length(brk)
  plot(AB$value, main="TINDVIBeforeMax", breaks=brk, col=rev(terrain.colors(nbrk)), axis.arg=list(at=seq(0,6,by=2), labels=seq(0,6,by=2)), zlim=c(0,6))
  writeRaster(AB$value, "TINDVIBeforeMax.img", overwrite=TRUE)
  
  
  AT=rasterFromXYZ(Area_Total)
  crs(AT)<-crs(ras)
  brk=seq(0,8, by=0.001)
  nbrk=length(brk)
  plot(AT$value, main="TINDVI", breaks=brk, col=rev(terrain.colors(nbrk)), axis.arg=list(at=seq(0,8,by=2), labels=seq(0,8,by=2)), zlim=c(0,8))
  writeRaster(AT$value, "TINDVI.img", overwrite=TRUE)
  
  
  As=rasterFromXYZ(Asymmetry)
  crs(As)<-crs(ras)
  brk=seq(-6,6, by=0.0001)
  nbrk=length(brk)
  plot(As$value, main="Asymmetry", breaks=brk, col=rev(terrain.colors(nbrk)), axis.arg=list(at=seq(-6.0,6.0,by=3), labels=seq(-6.0,6.0,by=3)), zlim=c(-6,6))
  writeRaster(As$value, "Asymmetry.img", overwrite=TRUE)
  
  Amp=rasterFromXYZ(Amplitude)
  crs(Amp)<-crs(ras)
  brk=seq(0,0.7, by=0.0001)
  nbrk=length(brk)
  plot(Amp$value, main="Amplitude", breaks=brk, col=rev(terrain.colors(nbrk)), axis.arg=list(at=seq(0,0.7,by=0.1), labels=seq(0,0.7,by=0.1)), zlim=c(0,0.7))
  writeRaster(Amp$value, "Amplitude.img", overwrite=TRUE)
 
  
  
  ##########################====================================##########################
  
  return("*********************Output file saved at working directory*************************")
  
  ##########################====================================##########################
}  
#' @export
#' @return Multiple time series curves together at the plot pannel
#' 
#' @param N - number of intersted points
#' @param Id1 -  ID number for point 1
#' @param Id2 -  Id number for point 2
#' @param Id3 -  ID number for point 3
#' @param Id4 -  ID number for point 4
#' @param Id5 -  ID number for point 5
#' @title Time series curves for Multiple points in the Region of Interest
#' @description MultiPointsPlot function takes the ID for the pixels within the region of interst and returns, the timeseries curves from these points, ploted together. The Id numbers can be obtained from the txt file (AllPixels.txt) outputs.
#' @keywords Curve from multiple points 
#' @keywords time-series curves
#' @author Sofanit Araya
#' 
#' @details This function allows plotting time series curves from multiple points together in a single plot which helps understanding the growth variability across the field.This inforaiton can further analyzed to provide insight on other environemtal factors.
#' @details The maximum number of pixeles allowed plotting togther are 5 points.
#' 
#' @examples MultiPointsPlot(3,11,114,125)
#' 
#' 
#' 
#' 
#' @seealso PhenoMetrics()
#' 
MultiPointsPlot<- function (N,Id1,Id2,Id3,Id4,Id5){
  #AP=read.table("Allpixels.txt")
  AP=read.table("AllPixels.txt", header=TRUE, sep=",", strip.white = TRUE)
  APP=as.matrix(AP[Id1,])
  #print (APP)
  par(mfrow=c(1,1))
  
  if (N>5){
    warning ('The maximum No of pixel to plot is 5')
    
    
    if ((is.numeric(Id1)==FALSE) || (is.numeric(Id2)==FALSE) || (is.numeric(Id3)==FALSE) || (is.numeric(Id4)==FALSE) || (is.numeric(Id5)==FALSE) ){
      stop ('ID should be numeric')
    }
    
    
    if (missing (Id1) | missing(Id2) | missing (Id3) | missing (Id4) | missing (Id5)){
      stop('Id missed')
    }
    ts.plot((ts(as.matrix(AP[Id1,])[4:length(APP)])), (ts(as.matrix(AP[Id2,])[4:length(APP)])), (ts(as.matrix(AP[Id3,])[4:length(APP)])), (ts(as.matrix(AP[Id4,])[4:length(APP)])), (ts(as.matrix(AP[Id5,])[4:length(APP)])),  ylim=c(0,1), , col=1:5)
    axis(2, at=seq(0,1,by=0.1))
    
  }
  
  
  if (N==1){
    warning('only one pixel ploted')
    
    
    if ((is.numeric(Id1)==FALSE) ){
      stop ('ID should be numeric')
    }
    
    
    
    if ((Id1>length(AP$T1))){
      stop ('Id out of range')
    }
    
    ts.plot ((ts(as.matrix(AP[Id1,])[4:length(APP)])), ylim=c(0,1))
    axis(2, at=seq(0,1,by=0.1))
  }
  
  if (N==2){
    if (missing (Id1) || missing(Id2)){
      stop('Id missed')
    }
    
    
    if ((is.numeric(Id1)==FALSE) || (is.numeric(Id2)==FALSE) ){
      stop ('ID should be numeric')
    }
    
    
    if ((Id1>length(AP$T1)) || (Id2>length(AP$T1))){
      stop ('Id out of range')
    }
    
    ts.plot ((ts(as.matrix(AP[Id1,])[4:length(APP)])), (ts(as.matrix(AP[Id2,])[4:length(APP)])), ylim=c(0,1), col=1:2)
    axis(2,  at=seq(0,1,by=0.1))
  }
  if (N==3){
    if ((missing (Id1)) || (missing(Id2)) || (missing (Id3))){
      stop ("Id missed")
    }
    
    
    if ((is.numeric(Id1)==FALSE) || (is.numeric(Id2)==FALSE) || (is.numeric(Id3)==FALSE) ){
      stop ('ID should be numeric')
    }
    
    
    if ((Id1>length(AP$T1)) || (Id2>length(AP$T1)) || (Id3>length(AP$T1))){
      stop ('Id out of range')
    }
    
    ts.plot ((ts(as.matrix(AP[Id1,])[4:length(APP)])), (ts(as.matrix(AP[Id2,])[4:length(APP)])), (ts(as.matrix(AP[Id3,])[4:length(APP)])), ylim=c(0,1), col=1:3)
    axis(2,  at=seq(0,1,by=0.1))
  }
  
  if (N==4){
    if (missing (Id1) || missing(Id2) || missing (Id3) || missing (Id4)){
      stop('Id missed')
    }
    
    
    if ((is.numeric(Id1)==FALSE) || (is.numeric(Id2)==FALSE) || (is.numeric(Id3)==FALSE) || (is.numeric(Id4)==FALSE) ){
      stop ('ID should be numeric')
    }
    
    
    if ((Id1>length(AP$T1)) || (Id2>length(AP$T1)) || (Id3>length(AP$T1)) || (Id4>length(AP$T1))){
      stop ('Id out of range')
    }
    
    ts.plot ((ts(as.matrix(AP[Id1,])[4:length(APP)])), (ts(as.matrix(AP[Id2,])[4:length(APP)])), (ts(as.matrix(AP[Id3,])[4:length(APP)])), (ts(as.matrix(AP[Id4,])[4:length(APP)])),  ylim=c(0,1), col=1:4)
    axis(2, at=seq(0,1,by=0.1))
  }
  if (N==5){
    if (missing (Id1) || missing(Id2) || missing (Id3) || missing (Id4) || missing (Id5)){
      stop('Id missed')
    }
    
    
    if ((is.numeric(Id1)==FALSE) || (is.numeric(Id2)==FALSE) || (is.numeric(Id3)==FALSE) || (is.numeric(Id4)==FALSE) || (is.numeric(Id5)==FALSE) ){
      stop ('ID should be numeric')
    }
    
    
    if ((Id1>length(AP$T1)) || (Id2>length(AP$T1)) || (Id3>length(AP$T1)) || (Id4>length(AP$T1)) || (Id5>length(AP$T1)) ){
      stop ('Id out of range')
    }
    
    ts.plot ((ts(as.matrix(AP[Id1,])[4:length(APP)])), (ts(as.matrix(AP[Id2,])[4:length(APP)])), (ts(as.matrix(AP[Id3,])[4:length(APP)])), (ts(as.matrix(AP[Id4,])[4:length(APP)])), (ts(as.matrix(AP[Id5,])[4:length(APP)])), ylim=c(0,1),  col=1:5)
    axis(2, at=seq(0,1,by=0.1))
  }
  return ("..........Curves ploted............................")
}