library(hexSticker)

img <- "candidates/PTTR_MERGE.png"

p <- sticker(img, package = "",
             s_x = 1, s_y = 1.05, s_width = 0.8,
             h_fill="#000000", h_color="#ffaa00",
             h_size = 1.2,
             l_x = 0.73, l_y = 1.42,
             #l_x =c(0.2, 0.2, 1.8, 1.8, 1, 1),
             #l_y=c(1.5, 0.48, 1.5, 0.48, 1.85, 0.1),
             l_width = 3.8,
             l_alpha = 0.4, spotlight = T,
             filename = "../logo_ori.png")
plot(p)

# bash: convert logo_ori.png -resize 130x150 logo.png
