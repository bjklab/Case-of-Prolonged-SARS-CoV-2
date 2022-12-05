#' ##############################
#' load libraries and set seed
#' ############################## 
library(webshot2)

set.seed(16)



#' ##############################
#' take shots
#' ############################## 

# Najjar-Debbiny et al
webshot2::webshot("https://academic.oup.com/cid/advance-article/doi/10.1093/cid/ciac443/6599020",
                  file = "./figs/najjar_debbiny.png", cliprect = "viewport")

# webshot2::webshot("https://academic.oup.com/cid/advance-article/doi/10.1093/cid/ciac443/6599020",
#                   file = "./figs/najjar_debbiny_full.png", vwidth = 1600, vheight = 9000, cliprect = "viewport")


# Ford et al
webshot2::webshot("https://academic.oup.com/cid/advance-article/doi/10.1093/cid/ciac868/6795102",
                  file = "./figs/ford.png", cliprect = "viewport")

# webshot2::webshot("https://academic.oup.com/cid/advance-article/doi/10.1093/cid/ciac868/6795102",
#                   file = "./figs/ford_full.png", vwidth = 1600, vheight = 9000, cliprect = "viewport")
