# 1. PACKAGES

if(
    !require(
        "pacman"
    )
){
    install.packages(
        "pacman"
    )
}

pacman::p_load(
    geodata,
    elevatr,
    terra, sf,
    tidyverse,
    classInt,
    rayshader,
    magick
)

# 2. GET COUNTRY & REGION POLYGONS
#---------------------------------

regions_sf <- geodata::gadm(
    country = "ITA",
    level = 1,
    path = getwd()
) |>
sf::st_as_sf()

plot(sf::st_geometry(region_sf))
print(regions_sf, n = 20)

region_sf <- subset(
    regions_sf,
    NAME_1 == "Sicily"
)

plot(sf::st_geometry(region_sf))

country_sf <- sf::st_union(regions_sf)
plot(sf::st_geometry(country_sf))

# 3. DEM
#-------

elev <- elevatr::get_elev_raster(
    locations = region_sf,
    z = 9, clip = "locations"
)

crs_lambert <- "+proj=laea +lat_0=52 +lon_0=10 +x_0=4321000 +y_0=3210000 +datum=WGS84 +units=m +no_defs"

elev_lambert <- elev |>
    terra::rast() |>
    terrra::project(crs_lambert)

elev_df <- as.data.frame(
    elev_lambert,
    xy = TRUE
)

names(elev_df)[3] <- "dem"

# 4. 2D MAP
#----------

breaks <- classInt::classIntervals(
    elev_df$dem,
    n = 7,
    style = "pretty"
)$brks

colors <- hcl.colors(
    n = length(breaks),
    palette = "Zissou 1"
)

texture <- colorRampPalette(
    colors
)(512)

map <- ggplot(data = elev_df) +
    geom_raster(
        aes(
            x = x,
            y = y,
            fill = dem
        )
    ) +
    scale_fill_gradientn(
        name = "Elevation (m)",
        colors = texture,
        breaks = breaks,
        labels = round(breaks, 0),
        limits = c(
            min(elev_df$dem),
            max(elev_df$dem)
        )
    ) +
    guides(
        fill = guide_colorbar(
            direction = "vertical",
            barheight = unit(30, "mm"),
            barwidth = unit(3, "mm"),
            title.position = "top",
            label.position = "right",
            title.hjust = .5,
            label.hjust = .5,
            ncol = 1,
            byrow = FALSE
        )
    ) +
    coord_sf(
        crs = crs_lambert
    ) +
    theme_minimal() +
    theme(
        axis.line = element_blank(),
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        legend.text = element_text(
            size = 9, color = "grey10"
        ),
        legend.title = element_text(
            size = 10, color = "grey10"
        ),
        legend.position = c(.1, .85),
                panel.grid.major = element_line(
            color = "white"
        ),
        panel.grid.minor = element_line(
            color = "white"
        ),
        plot.background = element_rect(
            fill = "white", color = NA
        ),
        legend.background = element_rect(
            fill = "white", color = NA
        ),
        panel.border = element_rect(
            fill = NA, color = "white"
        ),
        plot.margin = unit(
            c(0, 0, 0, 0), "lines"
        )
    )

# 5. RENDER SCENE
#----------------

h <- nrow(elev_lambert)
w <- ncol(elev_lambert)

rayshader::plot_gg(
    ggobj = map,
    width = w / 500,
    height = h / 500,
    scale = 50,
    solid = FALSE,
    shadow = TRUE,
    shadowcolor = "white",
    shadowwidth = 0,
    shadow_intensity = 1,
    zoom = .7,
    phi = 87,
    theta = 0,
    window.size = c(w / 4, h / 4)
)

# 6. RENDER OBJECT
#-----------------

url <- "https://dl.polyhaven.org/file/ph-assets/HDRIs/hdr/4k/photo_studio_loft_hall_4k.hdr"
hdri_file <- basename(url)

download.file(
    url = url,
    destfile = hdri_file,
    mode = "wb"
)

filename <- "3d-dem-sicily.png"

rayshader::render_highquality(
    filename = filename,
    preview = TRUE,
    light = TRUE,
    environment_light = hdri_file,
    intensity_env = .6,
    interactive = FALSE,
    width = w,
    height = h
)

# 7. BOUNDING BOX
#----------------

region_bbox <- sf::st_as_sfc(
    sf::st_bbox(
        region_sf
    )
)

country_bbox <- sf::st_as_sfc(
    sf::st_bbox(
        country_sf
    )
)

# 8. INSET MAP
#-------------

inset_map <- ggplot() +
    geom_sf(
        data = country_sf,
        color = "white",
        fill = "grey80",
        linewidth = .1
    ) +
    geom_sf(
        data = country_bbox,
        fill = "transparent",
        color = "grey80",
        linewidth = 1
    ) +
    geom_sf(
        data = region_bbox,
        fill = "transparent",
        color = "red",
        linewidth = 1.5
    ) +
    theme_void()

file_name <- "inset_map.png"

ggsave(
    filename = file_name,
    plot = inset_map,
    width = w / 500,
    height = h / 500,
    units = "in"

)

# 9. PUT MAPS TOGETHER
#---------------------

inset_map_img <- magick::image_read(
    file_name
)

region_dem_img <- magick::image_read(
    filename
)

inset_map_rescaled_img <- magick::image_scale(
    inset_map_img, "x1200"
)

final_img <- magick::image_composite(
    region_dem_img,
    inset_map_rescaled_img,
    gravity = "southeast",
    offset = "+0+0"
)

magick::image_write(
    final_img,
    "final_img.png"
)
