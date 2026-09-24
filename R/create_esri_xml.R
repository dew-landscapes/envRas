create_esri_xml  <- function(tif_path
                             , scale
                             , offset
                             ) {
  
  xml_content <- sprintf(
    '<?xml version="1.0" encoding="UTF-8"?>
<Metadata>
  <Esri>
    <RasterProperties>
      <RasterFiltering>
        <Scale>%f</Scale>
        <Offset>%f</Offset>
      </RasterFiltering>
    </RasterProperties>
  </Esri>
</Metadata>'
    , scale
    , offset
    )
  
  # Esri requires the extension to be exactly .tif.xml
  xml_path <- paste0(tif_path, ".xml")
  writeLines(xml_content, xml_path)
  
}