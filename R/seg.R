
## idx_df <- index2df("gossiping",
##                    search_term = '林飛帆',
##                    search_page = 1:5)

## pttR::down_html(as_url(idx_df$link), dir = "local/fan/")

#post_df <- post2df(list.files('local/fan', full.names = T))


#library(jiebaR)
#seg <- worker(bylines = T, symbol = T,
#              user = system.file('pttdict/pttdict.csv', package = 'pttR'))
#new_user_word(seg, "这是一个新词", "n")
#segged_content <- segment(post_df$content, seg)
