#! /usr/bin/env Rscript
dirlocation <- "data/French/"
require(readr)
library(ggplot2)
library(ggnewscale)

lexique <- read_delim(paste0(dirlocation, 'Lexique383.tsv'), delim='\t', col_types = paste0(c(rep("c", 6),
                                                                         rep("n", 4),
                                                                         "c",
                                                                         rep("n", 5),
                                                                         rep("c", 2),
                                                                         rep("n", 4),
                                                                         "c",
                                                                         "n",
                                                                         rep("c", 5),
                                                                         rep("n", 4),
                                                                         "c",
                                                                         "n"), 
                                                                       collapse = ""))

saveRDS(lexique, file=paste0(dirlocation, 'Lexique383.rds'))

bestletters <- list()
bestletters.frame <- data.frame(matrix(ncol=3, nrow=1))
colnames(bestletters.frame) <- c("letter", "letterposition", "wordlength")
for (nletter in 1:max(lexique$nblettres)){
  repertoire <- unique(lexique$ortho[lexique$nblettres==nletter])
  repertoire.frame <- do.call("rbind", strsplit(repertoire, ""))
  bestletters[[nletter]] <- apply(repertoire.frame, 2, FUN=function(x){
    names(which(table(x)==max(table(x))))
  })
  for (iletter in 1:nletter){
    newbestletters.frame <- data.frame(names(which(table(repertoire.frame[,iletter])==max(table(repertoire.frame[,iletter])))))
    colnames(newbestletters.frame) <- "letter"
    newbestletters.frame$letterposition <- iletter
    newbestletters.frame$wordlength <-nletter
    bestletters.frame <- rbind(bestletters.frame, newbestletters.frame)
  }
}

bestletters.frame <- bestletters.frame[!is.na(bestletters.frame$letter),]

gg <- ggplot(bestletters.frame[bestletters.frame$wordlength>1,])+
  geom_rect(aes(xmin=-Inf, xmax=Inf, ymin=-Inf, ymax=Inf, fill=wordlength), color=NA, show.legend = F)+
  scale_fill_gradient(low = "white", high = "grey50")+
  new_scale("fill")+
  geom_bar(aes(x=letterposition, y=1, fill=letter, group=letter), 
           stat='identity', position="stack", show.legend = F, width=1)+
  geom_hline(yintercept = c(0, Inf))+
  geom_text(aes(x=letterposition, y=1, label=letter, group=letter), stat='identity', position=position_stack(vjust = 0.5))+
  scale_x_continuous(expand = c(0,0), breaks = 1:max(bestletters.frame$wordlength))+
  scale_y_continuous(expand = c(0,0))+
  theme(axis.text.y = element_blank(), axis.ticks.y = element_blank(), panel.grid = element_blank(),
        panel.spacing = unit(0, "lines"))+
  facet_grid(vars(wordlength), scales = "free_y", space = "free_y")
gg

ggsave("pics/consensus-french-words.svg", width = 15, height=8)
