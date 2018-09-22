
library(ggplot2)
library(ggpubr) 

gp <- function(d_f, y_axe, y_lab, title='', scale=c('default')) {
  g <- ggplot(d_f, aes(y=y_axe, x=d_f$algorithm)) + # 
    geom_bar(stat="identity", aes(fill=algorithm),
             alpha=0.7, position="dodge", width=0.4) +
    geom_label(aes(label=round(y_axe, 3)), 
               size=2.2, colour="black", show.legend=F) + # group=d_f$algorithm, 
    scale_fill_manual(values=c("#CC9933", "#0033FF", "#99CC99", "#993333")) + # "#99CC99", "#33CC99", "#CCFFCC", "#33CC99"
    ylab(y_lab) + xlab('') # xlab == Algos
  
  if (length(scale) > 1 && scale != c('default')) {
    g <- g + scale_y_continuous(limits=range(scale), breaks=scale) 
  } 
  g + coord_flip() + ggtitle(title)
}

# customize the theme a bit
gtheme <- function(y_title=T, title=T, hjust=0.5, axis.text.y=element_blank()) {
  thm <- theme_classic() +
    theme(
      text = element_text(size=9),
      panel.background = element_rect(fill="azure1", colour="black",
                                      size=0.5, linetype="solid"),
      strip.background = element_rect(fill="lightskyblue3", colour="black",
                                      size=0.5),
      axis.text.y = axis.text.y
    )
  if (!y_title) {
    thm <- thm + 
      theme(axis.title.y = element_blank())
  }
  if (title) {
    thm <- thm + 
      theme(plot.title = element_text(color="black", size=12, face="bold", hjust=hjust))
  }
  else {
    thm <- thm + 
      theme(plot.title = element_text(color="transparent"))
  }
  return(thm)
}

gpWrapper <- function(df, scale=seq(0.0, 1.0, by=0.25)) {
  ## order matters
  gpl <- list(
    gp(df, df$acc_train, "Accuracy", title='Accuracy on Training Set', scale=scale),
    gp(df, df$acc_test, "Accuracy", title='Accuracy on Test Set', scale=scale),
    gp(df, df$kappa_train, "Kappa", title='Kappa on Training Set', scale=scale),
    gp(df, df$kappa_test, "Kappa", title='Kappa on Test Set', scale=scale),
    gp(df, df$oose, "Out-of Sample Error", title='Error on Test Set'),
    gp(df, df$utime, "User Time", title='User Time (seconds)')
  )
  return(gpl)
}

gpArrange <- function(gpl) {
  gga <- ggarrange(gpl[[1]] + gtheme(), 
                   gpl[[2]] + gtheme(),
                   gpl[[3]] + gtheme(),
                   gpl[[4]] + gtheme(),
                   gpl[[5]] + gtheme(),
                   gpl[[6]] + gtheme(),
                   ncol=2, nrow=3,
                   common.legend=T, legend="right")
  return(gga)
}
