
suppressPackageStartupMessages({
  library(dplyr)          # general data munging functionality
  library(sf)             # spatial objects and mapping
  library(leaflet)
  library(leaflet.extras)
  library(htmltools)
})

totitle <- function(x){ # convenience function for changing case of EPA ecoregion labels
    x <- stringr::str_to_title(x)
    gsub("Usa", "USA", x)
}

mapkey <- read.csv("data/noumenon/key.csv", colClasses="character", na.strings="") %>%   # import metadata and create HTML tag for referencing local audio files (when available)
    mutate(SRC=case_when(!is.na(XC) ~ sprintf("<audio src=\"%s\" type=\"audio/mp3\" autoplay controls></audio>", SRC),
                         is.na(XC) ~ NA_character_))

eco <- read_sf("data/noumenon/spatial/ecoregions") %>% st_transform(4326) %>%   # import spatial data (smoothed from raw EPA shapefile using Visvalingam/weighted area simplification at mapshaper.org)
    mutate( NOUMENON=mapkey$NOUMENON[match(NA_L2CODE,mapkey$NA_L2CODE)],        # match polygons to metadata: species name
            AUDIO=mapkey$SRC[match(NA_L2CODE,mapkey$NA_L2CODE)],                #                             audio HTML tag
            XC=mapkey$XC[match(NA_L2CODE,mapkey$NA_L2CODE)],                    #                             xeno-canto file ID
            RECORDIST=mapkey$RECORDIST[match(NA_L2CODE,mapkey$NA_L2CODE)],      #                             audio recordist
            ALPHA=mapkey$ALPHA[match(NA_L2CODE,mapkey$NA_L2CODE)],              #                             eBird species alpha code (for eBird URLs)
            LABEL=case_when(                                                    # create HTML popup labels
                !is.na(AUDIO) ~ sprintf(
                    "<b>%s</b><br/>
                    <a href=https://www.ebird.org/species/%s>%s</a><br/>
                    %s<br/>
                    <a href=https://xeno-canto.org/%s>%s</a>
                    <div style=\"float:right\">%s</div>", totitle(NA_L2NAME), ALPHA, NOUMENON, AUDIO, gsub("^XC","",XC), XC, RECORDIST
                ),
                NA_L2NAME != "WATER" ~ sprintf("<b>%s</b>", totitle(NA_L2NAME)), 
                TRUE ~ NA_character_
            ))

load("data/noumenon/spatial/political.RData")                                   # import state/province boundaries (from rnaturalearthhires)

epsg2163 <- leafletCRS(                                                         # create CRS for Lambert azimuthal equal-area projection
    crsClass = "L.Proj.CRS",
    code = "EPSG:2163",
    proj4def = "+proj=laea +lat_0=45 +lon_0=-100 +x_0=0 +y_0=0 +a=6370997 +b=6370997 +units=m +no_defs",
    resolutions = 2^(16:7)
)

er_pal <- colorFactor(mapkey$COLOR, domain=mapkey$NA_L2CODE, ordered=TRUE, na.color="#ffffff")      # create palette to key ecoregion IDs to fill colors

splash.box <- HTML(paste0(
  HTML(
    '<div class="modal fade" id="splashbox" role="dialog"><div class="modal-dialog"><!-- Modal content--><div class="modal-content"><div class="modal-header"><button type="button" class="close" data-dismiss="modal">&times;</button>'
  ), 
  
  HTML("NUMENA OF NORTH AMERICA"), 
  HTML(
    '</div><div class="modal-body">'
  ), 
  HTML(
    "<em>nu<b>&middot;</b>me<b>&middot;</b>non</em>
            <br/><br/>
            alt. <em>noumenon</em>, Greek <em>νοούμενον</em>, from <em>νοῦς</em>, meaning \"mind\", \"perception\", or \"sense\"
            <br/><br/>
            In the metaphysics of German philosopher Immanuel Kant, a numenon or <em>Ding an sich</em> (\"thing-in-itself\") is an entity the existence of which is not contingent upon human perception.
            Numena are contrasted with objects of the senses, that is, phenomena. In this sense, we regard numena as encapsulating the ineffable essence of a thing or, in this case, a place.
            <br/><br/>
            Click a region to hear the song of its people."
  ),
  
  HTML('</div><div class="modal-footer"><button type="button" class="btn btn-default" data-dismiss="modal">GOT IT!</button></div></div>')
  
))

# Define HTML for the infobox
info.box <- HTML(paste0(
  HTML(
    '<div class="modal fade" id="infobox" role="dialog"><div class="modal-dialog"><!-- Modal content--><div class="modal-content"><div class="modal-header"><button type="button" class="close" data-dismiss="modal">&times;</button>'
  ),
  
  # Header / Title
  HTML("NUMENA OF NORTH AMERICA"),
  HTML(
    '</div><div class="modal-body">'
  ),
  
  # Body
  HTML("<h4>An audiovisual guide to the avian soul of North America.</h4>
       From the minds of <a href=https://twitter.com/BenGFreeman1/status/1382011326784937984>Ethan Linck and Ben Freeman</a>, inspired by <a href=https://sora.unm.edu/sites/default/files/journals/condor/v039n01/p0009-p0010.pdf>Aldo Leopold's (1937)</a> description of \"the numenon of the Sierra Madre: the thick-billed parrot\".<br/>
                <br/><div style=\"float:left;\">
                Spatial data: <a href=https://www.epa.gov/eco-research/ecoregions-north-america>U. S. Environmental Protection Agency</a>
                <br/>
                <div style=\"float:left;\">
                Audio: <a href=https://www.xeno-canto.org>xeno-canto</a>
                </div>"),
  
  # Closing divs
  HTML('</div><div class="modal-footer"></div></div>')
))

# Define HTML for the infobox
splash.box <- HTML(paste0(
  HTML(
    '<div class="modal fade" id="splashbox" role="dialog"><div class="modal-dialog"><!-- Modal content--><div class="modal-content"><div class="modal-header"><button type="button" class="close" data-dismiss="modal">&times;</button>'
  ),
  
  # Header / Title
  HTML("NUMENA OF NORTH AMERICA"),
  HTML(
    '</div><div class="modal-body">'
  ),
  
  # Body
  HTML("<h4>An audiovisual guide to the avian soul of North America.</h4>
       From the minds of <a href=https://twitter.com/BenGFreeman1/status/1382011326784937984>Ethan Linck and Ben Freeman</a>, inspired by <a href=https://sora.unm.edu/sites/default/files/journals/condor/v039n01/p0009-p0010.pdf>Aldo Leopold's (1937)</a> description of \"the numenon of the Sierra Madre: the thick-billed parrot\".<br/>
                <br/><div style=\"float:left;\">
                Spatial data: <a href=https://www.epa.gov/eco-research/ecoregions-north-america>U. S. Environmental Protection Agency</a>
                <br/>
                <div style=\"float:left;\">
                Audio: <a href=https://www.xeno-canto.org>xeno-canto</a>
                </div>"),
  
  # Closing divs
  HTML('</div><div class="modal-footer"></div></div>')
))

leaflet(eco, options=leafletOptions(crs=epsg2163)) %>% 
  addBootstrapDependency() %>% 
  htmlwidgets::prependContent(tags$style(".leaflet-container {background:#ffffff; }")) %>% 
  setView(lng=-101.2996, lat=47.1164, zoom=3) %>% 
  addPolygons(fillColor=~er_pal(NA_L2CODE), fillOpacity=1,            # ecoregion polygons
              color="#272727", weight=1, 
              popup=~LABEL) %>% 
  addPolygons(data=political, fill=FALSE,                             # state/province boundaries
              color="#666666", weight=1, dashArray="5") %>%
  addGraticule(interval=10, style=list(color="#a1def7", weight=0.5)) %>%  # lat/long lines
  addEasyButton(easyButton(
    icon = "fa-info", title = "Map Information", position="topright", 
    onClick = JS("function(btn, map){ $('#infobox').modal('show'); }")
  )) %>% # Trigger the infobox
  htmlwidgets::appendContent(info.box) %>%
  # htmlwidgets::appendContent(splash.box, info.box) %>% # <--- Seems not to work with 2 appendices...splash.box must come first, but 
                                                         #        when loading info.box then goes dark
  htmlwidgets::onRender("
    function(map){ $('#splashbox').modal('show'); }
  ")
