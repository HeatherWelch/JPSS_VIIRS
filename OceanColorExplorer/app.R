### app for chla

#### code used to create smaller size rasters by reducing resolution and using GTiff
# for(file in list.files(pattern=".grd")){
#   print(file)
#   a=raster(file)
#   b=aggregate(a,fact=3)
#   name=gsub(".grd","",file)
#   writeRaster(b,paste0("/Users/heatherwelch/Dropbox/JPSS/JPSS_VIIRS/OceanColorExplorer/data/global_rasters_netcdf/",name),format="GTiff", options=c("COMPRESS=DEFLATE", "PREDICTOR=3", "ZLEVEL=6"),overwrite=T)
# }

##### Defining global objects####
# source functions
library(shinyalert)
library(shiny)
library(ggplot2)
library(magrittr)
library(lubridate)
library(leaflet.extras)
library(shinydashboard)
library(RColorBrewer)
library(raster)
library(tidyverse)
library(rgdal)

# modis=list.files("data/global_rasters_mask",pattern="erdMH1chlamday") %>% grep(".grd",.,value=T) 
# occci=list.files("data/global_rasters_mask/",pattern="ESACCI-OC-L3S") %>% grep(".grd",.,value=T) 
# avw=list.files("data/global_rasters_mask/",pattern="GlobColour_Merged_AVW") %>% grep(".grd",.,value=T) 
# gsm=list.files("data/global_rasters_mask/",pattern="GlobColour_Merged_GSM") %>% grep(".grd",.,value=T) 
# viirs=list.files("data/global_rasters_mask/",pattern="nesdisVHNSQchlaMonthly") %>% grep(".grd",.,value=T) 


ui <- dashboardPage(skin="green",
                    dashboardHeader(
                      title = "Ocean Color Explorer",
                      titleWidth = 420
                    ),
                    dashboardSidebar(
                      width = 250,
                      sliderInput("slider1","Select year",min=as.Date("2012","%Y"),max=as.Date("2018","%Y"),value=as.Date("2017","%Y"),timeFormat = "%Y",width = "100%"),
                      div(style="text-align:center;",actionButton(style="background-color:white;border-radius:15px;",
                                                                  inputId = "submit_loc",
                                                                  label = h4(style="text-align:center;color:blue;background-color:white;",tags$b("Calculate timeseries"))))
                      ),
                    
                    dashboardBody(
                      fluidRow(useShinyalert()
                      ),
                      fluidRow(
                        column(h4(div(style="text-align:center",tags$b("Select study area"))),width=4,leafletOutput("map16",height = 400)),
                        column(h2(" "),width = 8,plotOutput("timeseries"))
                        
                      )
                    ))


server <- shinyServer(function(input, output) {
  
  # options(rsconnect.max.bundle.size=5000000000) 
  # options(rsconnect.http = "curl")
  # 
  full_ext<-list()
  full_ext$lon<-(-119)
  full_ext$lat<-40.078259
  full_ext$zoom<-5

    shinyalert(
      title = "Welcome to the Ocean Color Explorer",
      text = "Please select study area and year to begin. Note, calculation will take a long time for a large area.",
      closeOnEsc = TRUE,
      closeOnClickOutside = TRUE,
      html = FALSE,
      type = "success",
      showConfirmButton = TRUE,
      showCancelButton = FALSE,
      confirmButtonText = "OK",
      confirmButtonCol = "#AEDEF4",
      timer = 0,
      imageUrl = "",
      animation = TRUE
    )

  
  output$map16 <- renderLeaflet({
    print(input$slider1)
    year=input$slider1 %>% strtrim(.,4)
    print(year)
    
    lmap <- leaflet()
    lmap <- setView(lmap, 0, 0, 1)
    lmap <- addProviderTiles(lmap, "Esri.NatGeoWorldMap",options = providerTileOptions(noWrap = TRUE))
    lmap <- addDrawToolbar(lmap,
      targetGroup = "draw",
      polylineOptions=F,
      polygonOptions=F,
      circleOptions=F,
      markerOptions=F,
      circleMarkerOptions=F,
      editOptions = F,
      singleFeature =T)
    
    lmap
    
  })
  
  coords=eventReactive(input$map16_draw_new_feature,{
    feature <- input$map16_draw_new_feature
    ymax=feature$geometry$coordinates[[1]][[3]][[2]]
    ymin=feature$geometry$coordinates[[1]][[4]][[2]]
    xmax=feature$geometry$coordinates[[1]][[3]][[1]]
    xmin=feature$geometry$coordinates[[1]][[5]][[1]]
    coordinates=extent(xmin, xmax, ymin, ymax)
    
    print(ymax)
    print(ymin)
    print(xmax)
    print(xmin)
    
    return(coordinates)
    
  })
  
  data=eventReactive(input$submit_loc,{

    withProgress(message = 'Calculating timeseries: ', value = 0, {
      incrs <- 7
      step <-1
      rstack<-c()
      incProgress(1/incrs, detail = paste("processing..."))
      
    year=input$slider1 %>% strtrim(.,4)
    rasterlist=list.files("data/global_rasters_netcdf",pattern = ".tif",full.names = F) %>% grep(year,.,value=T)
    rasterlist_paths=list.files("data/global_rasters_netcdf",pattern = ".tif",full.names = T) %>% grep(year,.,value=T)
    print(rasterlist)
    df=data.frame(number=1:length(rasterlist)) %>% mutate(date=as.Date("2012-03-01"))%>% mutate(sensor=NA)%>% mutate(mean=NA)
    for(i in 1:length(rasterlist)){
      name=rasterlist[i]
      ras=rasterlist_paths[i] %>% raster()
      ras=crop(ras,coords())
      # ras=crop(ras,e)
      
      if(grepl("erdMH1chlamday", name)){
        sensor="MODIS"
        date=substr(name,16,25) %>% as.Date()
      }
      
      if(grepl("nesdisVHNSQchlaMonthly", name)){
        sensor="VIIRS"
        date=substr(name,24,33) %>% as.Date()
      }
      
      if(grepl("AVW", name)){
        sensor="GlobColour Merged AVW"
        date=substr(name,23,32) %>% as.Date()
      }
      
      if(grepl("GSM", name)){
        sensor="GlobColour Merged GSM"
        date=substr(name,23,32)%>% as.Date()
      }
      
      if(grepl("ESACCI-OC-L3S", name)){
        sensor="OC-CCI"
        date=substr(name,15,24) %>% as.Date()
      }
      
      mean=log(ras+0.001) %>% cellStats(.,stat="mean",na.rm=T)
      df$date[i]=date
      df$sensor[i]=sensor
      df$mean[i]=mean
      
    }
    print(df)
    # df$mean[is.nan(df$mean)] <- NULL
    # new=df[complete.cases(df),]
    df$sensor=as.factor(df$sensor)
    return(df)
    })
    
  })
  
  output$timeseries=renderPlot({
    year=input$slider1 %>% strtrim(.,4)
    # new=df %>% .[complete.cases(.),]
    # new$mean[is.nan(new$mean)] <- NA
    a=ggplot(data(),aes(x=date,y=mean)) +geom_line(aes(group=sensor,color=sensor,linetype=sensor),size=.5)+geom_point(aes(color=sensor))+
      scale_x_date(date_breaks="month",date_labels = "%m",date_minor_breaks = "months")+
      theme(legend.position=c(.9,.9),legend.justification = c(.4,.4))+
      theme(axis.text = element_text(size=10),axis.title = element_text(size=10),legend.text=element_text(size=10),legend.title = element_text(size=10),strip.text.y = element_text(size = 10),strip.text.x = element_text(size = 10), strip.background = element_blank())+
      theme(legend.key.size = unit(.5,'lines'))+theme_bw()+ggtitle(paste0("Comparison of log chlorophyll concentrations for ", year))+
      scale_color_manual("Product",values=c("VIIRS"="#d3ad06","MODIS"="#0066cc","OC-CCI"="red","GlobColour Merged GSM"="black","GlobColour Merged AVW"="darkgreen"))+ylab("log Chl-a (mg/m3)")+xlab("Month")+
      scale_linetype_manual("Product",values = c("VIIRS"="dashed","MODIS"="dashed","OC-CCI"="solid","GlobColour Merged GSM"="solid","GlobColour Merged AVW"="solid"))
    
    a
  })
  
})


shinyApp(ui = ui, server = server)