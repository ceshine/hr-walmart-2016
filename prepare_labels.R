pacman::p_load(data.table)
pacman::p_load(foreach)

# Load training Data
train.dataset = fread("data/train.tsv", sep="\t")
print(paste("Train dataset shape:", paste(dim(train.dataset), collapse=",")))


# Parse Tag
parseTag <- function(str, ret_list=F){
    len = nchar(str)
    if(ret_list) return(strsplit(substr(str, 2, len-1), split=", "))
    return(unlist(strsplit(substr(str, 2, len-1), split=", ")))
}
tag.all = unique(
    foreach(str=train.dataset[,unique(tag)], .combine="c") %do% parseTag(str))

for(tagname in tag.all){
    train.dataset[, eval(tagname) := sapply(tag, grepl, pattern=eval(tagname)) * 1]
}

tag.cnt = melt(train.dataset[, tag.all, with=F][, lapply(.SD, sum)], measure.vars=tag.all)
setorderv(tag.cnt, c("value"), -1)
# tag.cnt[, perc := value / nrow(train.dataset)]
# ggplot(tag.cnt, aes(x=value)) + geom_histogram(bins=20)

# Discard less frequent tags
tag.cnt = tag.cnt[value >= 20, ]

# Save tags for later use
fwrite(train.dataset[, c("item_id", as.character(tag.cnt[,variable])), with=F], "data/label.csv")