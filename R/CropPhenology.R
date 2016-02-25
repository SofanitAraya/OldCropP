

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
#' @author   Sofanit Araya
#' @keywords Phenology, remote sensing, satellite image
#' @seealso TwoPointsPlot (Id1, Id2)
#' @description This function extracts major phenologic parameters from time series vegetaion index data. Total of 11 phenologic metrics as raster and Ascii files. The function takes path of the vegetation index data and the boolean Value for BolAOI (True- if there is AOI polygon, FALSE- if the parameters are calculated for the whole region).
#' @param Rawpath - Text value - the path where the time series images saved 
#' @param BolAOI-  Logical value - if there is any area of intererst or not
#' @export
#' @details
#'Remote sensing phenology also called Land Surface Phenology refers to observation of seasonal pattern of vegetation changes using remote sensing vegetation indices (Reed etal. 2009).
#'Remotely sensed vegetation phenology has been used for many ecological studies including as an indicator for climate change (Kramer et al.,2000; White et al., 1997), to estimate agricultural productivity (Hill and Donald, 2003; Labus et al., 2002; Sakamoto et al., 2013), regional management for crop type mapping (Brown et al., 2013; Niazmardi et al., 2013), as an indicator of soil Plant Available Water Holding Capacity (PAWC) variability across a farm (Araya etal. 2015)  and many more applications. 
#'Different methods have been employed to extract phenologic metrics, which include threshold definition (White et al., 1997), decomposition of the vegetation dynamic curve using harmonic analysis (Jakubauskas et al., 2001; Roerink et al., 2011) (Zhang et al., 2003) , taking the first derivative of the smoothed and non-smoothed vegetation index dynamics curves (Moulin et al., 1997) and defining the crossover point of the smoothed and non-smoothed dynamics curves (Hill and Donald, 2003; Reed et al., 1994). In this package the phenologic metrics were extracted based on correlation of the description of crop physiological stages (Zadoks etal. 1974) with the relative greenness of the crop on the vegetation dynamics. 
#'The list of the phenologic metrics and their description are provided at the application Note for this package (Araya etal. 2016- to be published)
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
#' @references  White, M.A., Thornton, P.E., Running, S.W., 1997. A continental phenology model for monitoring vegetation responses to interannual climatic variability. Glo. Biogeochem. Cyc. 11, 217-234.
#' @references Zadoks, J.C., Chang, T.T., Konzak, C.F., 1974. A decimal code for the growth stages of cereals. Weed Research 14, 415-421.


#' 
#' 
#' 
PhenoMetrics<- function (RawPath, BolAOI){
  
  # install.packages("shapefiles")
  #install.packages("raster")
  #install.packages("maptools")
  #install.packages("gdal")
  require('shapefiles')
  #library("shapefiles")
  require("raster")
  #library("raster")
  require("maptools")
  #library("maptools")
  require("rgdal")
  #library("rgdal")
  require("xlsx")
  
  setwd(RawPath)
  raDir=dir(path=RawPath, pattern="*.img$")
  shDir=dir(pattern="*.dbf$")
  FileLen=length(raDir)
  filelen=length(raDir)
  
  #f=system.file(raDir[1], package="raster")
  
  q=1
  qon=1
  qoff=1
  qmax=1
  #shd=rasterToPoints(raster(raDir[1]), spatial= FALSE)
  #shd=read.dbf(shDir[1])
  #shdb=shd
  
  par(mfrow=c(1,1))
  par(mar=c(3.5, 2.5, 2.5, 5.5))
  # r=length(shdb$GRID_CODE)
  s=1
  ## r counts number of pixels per image
  # q counts the file numbers under the directory
  #Onset=shdb$GRID_CODE[1]
  #AnnualTS=as.matrix(Onset)
  # s is pixel number
  # r is the number of values for each pixels - 20 for year 2000 and 23 for the other years 
  AOI=dir(pattern="*.shp$")
  shp=readShapePoly(AOI)
  
  
  i=1
  try=0
  
  while (i<(filelen+1)) {
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
  
  r=length(try[[1]][,"value"])
  
  Head="ID X-Cord Y_Cord T1 T2 T3 T4 T5 T6 T7 T8 T9 T10 T11 T12 T13 T14 T15 T16 T17 T18 T19 T20 T21 T22 T23"
  #write.xlsx(Head, file="All_Pixels.xls", append=TRUE, ncolumns=length(Head), sep=" ")
  
  while(s>0 & s<(r+1)){ #iterate through the each pixel
    
    #+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
    # Plot the time series curve of the year for the sth pixel
    #+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
    
    AnnualTS=as.matrix(0)
    q=1 #reset qon for the next pixel comparision
    #===================== Iterate throught the files for s-th pixels to get the curve
    
    while (q>0 & q<filelen+1){
      GRD_CD=(try[[q]][,"value"][s])/10000
      if (is.na(GRD_CD)){
        GRD_CD=(try[[q-1]][,"value"][s])/10000
      }
      AnnualTS[q]=GRD_CD
      q=q+1
    }
    cordinate=0
    cordinate[1]=cor[s,1]
    cordinate[2]=cor[s,2]
    
    dir.create("Result")
    setwd(paste(getwd(), "Result", sep="/"))
    
    write(append(s,append(cordinate,AnnualTS)), file="All.txt", append=TRUE, ncolumns=(length(AnnualTS)+3))
    ts.plot(AnnualTS)
    #s=s+1
    #AnnualTS is the time series of all the pixels
    #======================================
    
    
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
    min2=mean(AnnualTS[21:22]) #minimum after the amplitude
    range1=0.1*min1 #to get 20% of the min before Max
    range2=0.1*min2#to get 20% of the min after Max
    trsh1=min(AnnualTS[5:6])+range1 # to get 20% more greenness than the min before max
    trsh2=min(AnnualTS[21:22])+range2 # to get 20% more greenness than the min after max
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
    
    if (ls==5){
      if ((AnnualTS[12]-AnnualTS[11])< 0){
        Em=12
      }
      if (AnnualTS[12]-AnnualTS[11]>0){
        if (slop[ls-1]<0){
          Em=ls+7
        }
        if (slop[ls-1]>0){
          k=7
          while (k<12){
            if (AnnualTS[k]>trsh1){
              g=k-6
              if (slop[k-6]>(-0.01)){
                Em=k
                break
              }
            }
            k=k+1
          }
        }      
      }
      if (Em==0){
        Em=12
      } 
    }
    
    if ((ls<5)& (ls>2)){
      
      if (slop[ls-1]<(-0.01)){ # if the previous is -ve, i.e deep decreament
        Em=ls+6
      }
      touched=FALSE
      if (slop[ls-1]>(-0.01)){ #if it is minor decrease on genneral +ve trend
        k=7
        while (k<12){
          if (AnnualTS[k]>trsh1){ # check pt where the trushold passed
            g=k-6
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
    if((ls==1) | (ls==2)){
      k=ls+7
      tk=1
      while (k<12){
        if (AnnualTS[k]>trsh1){
          g=k-6
          if (slop[k-6]>(-0.01)){
            Em=k
            tk=tk+1
            break
          }
        }
        k=k+1
      }
      if (tk==1){ Em=12 }
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
    
    
    if (onsetV>1){
      onsetV=trsh1
      onset=Em
    }
    
    #==============================
    
    Onset_Value[,"value"][s]=onsetV*10000
    Onset_Time[,"value"][s]=onset
    print (s)
    print (r)
    print (onset)
    print(onsetV)
    
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
    #                                                  Offset
    #+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
    
    
    j=Max_T #J is the lower bound on down AnnualTS / upeer bound in sequence
    while (j<FileLen){
      if (AnnualTS[j]<trsh2){
        of=j
        break
      }
      j=j+1
    }
    
    of1=AnnualTS[j-1]-AnnualTS[j]
    of2=(trsh2-AnnualTS[of])
    x=of2/of1
    offset=of-x
    
    print (offset)
    print(trsh2)
    
    if ((max-trsh2)<0.05) {
      crp=FALSE
      offset=0
    }
    
    Offset_Value[,"value"][s]=trsh2
    Offset_Time[,"value"][s]= offset
    
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
    print (Area)
    #if (start==end){ Area=0}
    
    Area_Total[,"value"][s]=Area
    
    
    start1=St
    Area1=AnnualTS[start1]/2
    start1=start1+1
    while (start1<mx){
      Area1=Area1+AnnualTS[start1]
      start1=start1+1
    }
    Area1=Area1+AnnualTS[mx]/2
    print (Area1)
    if (Area==0){ Area1=0}
    Area_Before[,"value"][s]=Area1
    
    Area2=AnnualTS[mx]/2
    start2=mx+1
    while (start2<(end)){
      Area2=Area2+AnnualTS[start2]
      start2=start2+1
    }
    Area2=Area2+AnnualTS[Ed]/2
    print (Area2)
    if (Area==0){ Area2=0}
    Area_After[,"value"][s]=Area2
    
    Asy=Area1-Area2
    Asymmetry[,"value"][s]=Asy
    
    s=s+1
    
  }
  
#  dir.create("Result4")
#  setwd(paste(getwd(), "Result4", sep="/"))
  
  
  write.table(Area_Total, "Area_2003.txt")
  write.table(Area_After, "Area_2003_After.txt")
  write.table(Area_Before, "Area_2003_Before.txt")
  
  write.table(Max_Value, "Max_V_2003.txt")
  write.table(Max_Time, "Max_T_2003.txt")
  
  write.table(Offset_Value, "Offset_2003.txt")
  write.table(Offset_Time, "Offset_T_2003.txt")
  
  write.table(Onset_Value, "Onset_2003.txt")
  write.table(Onset_Time, "Onset_T_2003.txt")
  
  write.table(Asymmetry, "Asymmetry.txt")
  ###===================================================================================================
  #Defining secondary metrics
  
  GreenUpSlope=Max_Time
  GreenUpSlope[,"value"]=(Max_Value[,"value"]-(Onset_Value[,"value"]/10000))/(Max_Time[,"value"]-Onset_Time[,"value"])
  write.table(GreenUpSlope, "GreenUpSlope.txt")
  
  BrownDownSlope=Max_Time
  BrownDownSlope[,"value"]=(Max_Value[,"value"]-Offset_Value[,"value"])/(Offset_Time[,"value"]-Max_Time[,"value"])
  write.table(BrownDownSlope, "BrownDownSlope.txt")
  
  LengthGS=Max_Time
  LengthGS[,"value"]=(Offset_Time[,"value"]-Onset_Time[,"value"])
  write.table(LengthGS, "LengthGS.txt")
  
  
  
  ###===================================================================================================
  par(mfrow=c(2,2))
  
  MT=rasterFromXYZ(Max_Time)
  crs(MT)<-crs(ras)
  plot(MT$value, main="Max Time")
  writeRaster(MT$value, "Max_T.img", overwrite=TRUE)
  
  MV=rasterFromXYZ(Max_Value)
  crs(MV)<-crs(ras)
  plot(MV$value, main="Max_NDVI")
  writeRaster(MV$value, "Max_V.img", overwrite=TRUE)
  
  AT=rasterFromXYZ(Area_Total)
  crs(AT)<-crs(ras)
  plot(AT$value, main="Total area")
  writeRaster(AT$value, "Area_Total.img", overwrite=TRUE)
  
  AA=rasterFromXYZ(Area_After)
  crs(AA)<-crs(ras)
  plot(AA$value, main="Area After Max")
  writeRaster(AA$value, "Area_After.img", overwrite=TRUE)
  
  AB=rasterFromXYZ(Area_Before)
  crs(AB)<-crs(ras)
  plot(AB$value, main="Area Before Max")
  writeRaster(AB$value, "Area_Before.img", overwrite=TRUE)
  
  OT=rasterFromXYZ(Onset_Time)
  crs(OT)<-crs(ras)
  plot(OT$value, main="Onset Time")
  writeRaster(OT$value, "Onset_T.img", overwrite=TRUE)
  
  OV=rasterFromXYZ(Onset_Value)
  crs(OV)<-crs(ras)
  plot(OV$value, main="Onset_NDVI")
  writeRaster(OV$value, "Onset_V.img", overwrite=TRUE)
  
  OFT=rasterFromXYZ(Offset_Time)
  crs(OFT)<-crs(ras)
  plot(OFT$value, main="Offset Time")
  writeRaster(OFT$value, "Offset_T.img", overwrite=TRUE)
  
  OFV=rasterFromXYZ(Offset_Value)
  crs(OFV)<-crs(ras)
  plot(OFV$value, main="Offset_NDVI")
  writeRaster(OFV$value, "Offset_V.img", overwrite=TRUE)
  
  GUS=rasterFromXYZ(GreenUpSlope)
  crs(GUS)<-crs(ras)
  plot(GUS$value, main="GreenUpSlope")
  writeRaster(GUS$value, "GreenUpSlope.img", overwrite=TRUE)
  
  BDS=rasterFromXYZ(BrownDownSlope)
  crs(BDS)<-crs(ras)
  plot(BDS$value, main="BrownDownSlope")
  writeRaster(BDS$value, "BrownDownSlope.img", overwrite=TRUE)
  
  Len=rasterFromXYZ(LengthGS)
  crs(Len)<-crs(ras)
  writeRaster(Len$value, "LengthGS.img", overwrite=TRUE)
  plot(Len$value, main="GS Length")
  
  As=rasterFromXYZ(Asymmetry)
  crs(As)<-crs(ras)
  plot(As$value, main="Asymmetry")
  writeRaster(As$value, "Asymmetry.img", overwrite=TRUE)
  
  print(proc.time ()-ptm) 
  
  ##########################====================================##########################
  
  return("***********************Output file saved at working directory*******************************")
  
  ##########################====================================##########################
}  
#' @export
#' @return Multiple time series curves together in a single plot
#' @param N- number of intersted points
#' @param Id1 -  ID number for point 1
#' @param Id2 -  Id number for point 2
#' @param Id3 -  ID number for point 1
#' @param Id4 -  ID number for point 1
#' @param Id5 -  ID number for point 1
#' @title Time series curves for Multiple points in the Region of Interest
#' @description MultiPointsPlot function takes the ID for the pixels within the region of interst and returns, the timeseries curves from these points, ploted together. The Id numbers can be obtained from the txt file (All_Pixels.txt) outputs.
#' @keywords Curve from multiple points 
#' @keywords time-series curves
#' @author Sofanit Araya
#' 
#' @examples TwoPointsPlot(114,125)
#' 
#' 
#' # The function results multiple time series vegetation index curves together at the plot pannel
#' 
#' @seealso PhenoMetrics()
#' 
MultiPointsPlot<- function (N,Id1,Id2,Id3,Id4,Id5){
  AP=read.table("All_pixels.txt", header=TRUE)
  APP=as.matrix(AP[Id1,])
  if (N>5){
    warning ('The maximum No of pixel to plot is 5')
    if (missing (Id1) | missing(Id2) | missing (Id3) | missing (Id4) | missing (Id5)){
      stop('Id missed')
    }
    return (ts.plot(ts(as.matrix(AP[ID1,])[4:length(APP)])), (ts.plot(ts(as.matrix(AP[ID2,])[4:length(APP)]))), (ts.plot(ts(as.matrix(AP[ID3,])[4:length(APP)]))), (ts.plot(ts(as.matrix(AP[ID4,])[4:length(APP)]))), (ts.plot(ts(as.matrix(AP[ID5,])[4:length(APP)]))), col=1:5)
  }
  if (N==1){
    warning('only one pixel to be ploted')
    AP
    return (ts.plot(ts(as.matrix(AP[ID1,])[4:length(APP)])))
  }
  if (N==2){
    if (missing (Id1) || missing(Id2)){
      stop('Id missed')
    }
    return (ts.plot(ts(as.matrix(AP[ID1,])[4:length(APP)])), (ts(as.matrix(AP[ID2,])[4:length(APP)])), col=1:2)
  }
  if (N==3){
    if ((missing (Id1)) || (missing(Id2)) || (missing (Id3))){
      stop ("Id missed")
    }
    return (ts.plot(ts(as.matrix(AP[ID1,])[4:length(APP)])), (ts(as.matrix(AP[ID2,])[4:length(APP)])), (ts(as.matrix(AP[ID3,])[4:length(APP)])), col=1:3)
  }
  if (N==4){
    if (missing (Id1) || missing(Id2) || missing (Id3) || missing (Id4)){
      stop('Id missed')
    }
    return (ts.plot(ts(as.matrix(AP[ID1,])[4:length(APP)])), (ts(as.matrix(AP[ID2,])[4:length(APP)])), (ts(as.matrix(AP[ID3,])[4:length(APP)])), (ts(as.matrix(AP[ID4,])[4:length(APP)])), col=1:4)
  }
  if (N==5){
    if (missing (Id1) || missing(Id2) || missing (Id3) || missing (Id4) || missing (Id5)){
      stop('Id missed')
    }
    return (ts.plot(ts(as.matrix(AP[ID1,])[4:length(APP)])), (ts(as.matrix(AP[ID2,])[4:length(APP)])), (ts(as.matrix(AP[ID3,])[4:length(APP)])), (ts(as.matrix(AP[ID4,])[4:length(APP)])), (ts(as.matrix(AP[ID5,])[4:length(APP)])), col=1:5)
  }
  
  return ("..........The curves are here............................")

}