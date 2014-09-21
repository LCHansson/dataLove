load("examples/data/valdata.Rdata")

val2006R$valdeltagande <- as.character(val2006R$valdeltagande) %>% str_replace_all(",", ".") %>% as.numeric
val2014K$valdeltagande <- as.character(val2014K$valdeltagande) %>% str_replace_all(",", ".") %>% as.numeric
