library(hexSticker)

img <- "candidates/pttR_CH_BLACK_ansi.png"

p <- sticker(img, package = "",
             s_x = 1, s_y = 1, s_width = 0.7,
             h_fill="#000000", h_color="#f39c12")
plot(p)
