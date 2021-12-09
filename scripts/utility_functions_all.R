#define functions to read data from EDI
getCurrentVersion<-function(edi_id){
  require(magrittr)
  #edi_id="knb-lter-bnz.571"
  edi_id_slash=gsub('\\.', '/', edi_id)
  versions=readLines(paste0('https://pasta.lternet.edu/package/eml/', edi_id_slash), warn=FALSE)%>%
    as.numeric()%>%(max)
  packageid=paste0(edi_id, '.', versions)
  return (packageid)
}

#function to download the EML file from EDI
getEML<-function(packageid){
  require(magrittr)
  myurl<-paste0("https://portal.lternet.edu/nis/metadataviewer?packageid=",
                packageid,
                "&contentType=application/xml")
  #myeml<-xml2::download_html(myurl)%>%xml2::read_xml()%>%EML::read_eml()
  myeml<-xml2::read_xml(paste0("https://portal.lternet.edu/nis/metadataviewer?packageid=",
                               packageid,
                               "&contentType=application/xml"))%>%EML::read_eml()
}


#function to get a single element anywhere in an eml
eml_get_simple <- function(x, element, from = "list", ...){
  doc <- as.character(emld::as_json(emld::as_emld(x, from = from)))
  out <- jqr::jq(doc, paste0("..|.", element, "? // empty"))
  json <- jqr::combine(out)
  robj <- jsonlite::fromJSON(json, simplifyVector = FALSE)
  return(robj)
}

#function view all non-numeric
#define function
findNonNumeric<-function(x){
  y<-unique(suppressWarnings(x[is.na(as.numeric(x))]))
}


#extracts values from shapefile
add_shapefile_polygon = function(df, shapefilePath, shapefileData,
                                 crsPointFile="+init=epsg:4326"){
  require(rgdal)
  require (sp)
  shpfile=rgdal::readOGR(shapefilePath,verbose=TRUE)
  tmp<-df
  tmp$latitude<-as.numeric(as.character(tmp$latitude))
  tmp$longitude<-as.numeric(as.character(tmp$longitude))
  sp::coordinates(tmp)<-c('longitude', 'latitude')
  sp::proj4string(tmp) <- sp::CRS(crsPointFile) #SCE need to check if default is right for NPN
  shpfile<- spTransform(shpfile, CRS=raster::crs(tmp))
  ptspoly <- sp::over(tmp, shpfile)
  ptspoly<-ptspoly[names(ptspoly)%in%shapefileData]
  df<-cbind(df, ptspoly)
  return(df)
}
