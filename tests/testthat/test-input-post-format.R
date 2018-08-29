context("Test various possible input post formats")

## Posts with Author replies in comments
# "https://www.ptt.cc/bbs/Baseball/M.1535372407.A.902.html"
# "https://www.ptt.cc/bbs/car/M.1500309737.A.D47.html"

## Posts with No comments
# system.file("ntucourse/posts", "M.1534838066.A.27C.html.gz", package = "pttR")

files <- list.files(system.file("ntucourse/posts", package = "pttR"),
                    full.names = TRUE)

post_df <- post2df(files)



## Posts with cross-year comments
# "https://www.ptt.cc/bbs/Gossiping/M.1514735137.A.8BD.html"
