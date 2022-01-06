##created by Zaohuang Chen, 2022.1

##Usage





##library require: ggplot2; seqinr;dplyr;showtext

#Auto install the require library
if (!require("ggplot2")) install.packages("pacman")
if (!require("seqinr")) install.packages("seqinr")
if (!require("dplyr")) install.packages("dplyr")
if (!require("showtext")) install.packages("showtext")
if (!require("plotly")) install.packages("plotly")



#load the library
require(ggplot2)
require(seqinr)
require(dplyr)
require(showtext) # this library can deal with chinese!
require(plotly)

###plot all files that contain suffixes seem like c(".fa",".fas", ".fasta" ) in current directory

seq.files<-list.files()

for (seq.files in seq.files[grep(".fa$|.fas$|.fasta$", seq.files) ] ){

  ####### process fasta files#####
  seq.fa<-read.fasta(seq.files,seqtype = "DNA")

  # remove duplicated seq
  seq.fa<-seq.fa[!duplicated(names(seq.fa))]

  #cut the name of each sequence, leaving  only the first three prefixes

  names(seq.fa)<- unlist(lapply(strsplit(names(seq.fa),split = "_"), function(x){
    paste0(x[1],"_",x[2],"_",x[3])
  }),use.names = F)


  ## plot the DNA barcode
  cc<-lapply(seq.fa,function(x){
    bc=data.frame(group=as.character(x))
    bc <-bc %>%  dplyr::filter(group %in% c("a","g","c","t")) # remove any base not in c("a","g","c","t")
    bc$xpos<-rep_len(0:nrow(bc)*0.15,length.out = nrow(bc))
    return(bc)
  })

  cc<-bind_rows(cc,.id = "type")

  showtext_auto()
  p<-ggplot()+geom_rect(data=cc,aes(xmin=xpos,xmax=xpos+0.15,ymin=0,ymax=1,show.legend = F,fill=group))+
    facet_wrap(vars(type),ncol = 1,strip.position = "top")+
    theme_void()+scale_fill_manual(values = c("#d11141","#00b159","#ffc425","#00aedb"))+
    theme(strip.background = element_blank(),
          strip.text.x = element_text(hjust = 0.068,size = 6),
          legend.position = "none",
          panel.spacing = unit(1,"lines")) #increase facet_warp space

  pdf(paste0(seq.files,".pdf"),width = 8,height = length(table(cc$type))/2)
  plot(p)
  dev.off()


}



####
#bb<-lapply(seq.fa,function(x){
#  barcode(x)
#})


#ggpubr::ggarrange(bb[[1]],bb[[2]],ncol = 1)


#barcode = function(aa){
#  bc=data.frame(group=as.character(aa))
#  bc$xpos<-rep_len(0:nrow(bc)*0.15,length.out = nrow(bc))
#  ggplot()+geom_rect(data=bc,aes(xmin=xpos,xmax=xpos+0.15,ymin=0,ymax=1,show.legend = F,fill=group))+
#    theme_void()+scale_fill_manual(values = c("#d11141","#00b159","#ffc425","#00aedb"))+theme(legend.position = "none")+
#    geom_text(aes(label=attributes(aa)$name,x=2,y=1.2),size=5)
#}


