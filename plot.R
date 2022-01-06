##created by Zaohuang Chen, 2022.1

##Usage





##library require: ggplot2; seqinr;dplyr;showtext

#Auto install the require library
if (!require("ggplot2")) install.packages("pacman")
if (!require("seqinr")) install.packages("seqinr")
if (!require("dplyr")) install.packages("dplyr")
if (!require("showtext")) install.packages("showtext")

#load the library
require(ggplot2)
require(seqinr)
require(dplyr)
require(showtext) # this library can deal with chinese!


####### process fasta files#####
seq.fa<-read.fasta("~/Documents/DNAbarcode/附件2鱼类条形码序列文件-final-NOgap.fas",seqtype = "DNA")


# remove duplicated seq
seq.fa<-seq.fa[!duplicated(names(seq.fa))]

## plot the DNA barcode
cc<-lapply(seq.fa,function(x){
  bc=data.frame(group=as.character(x))
  bc$xpos<-rep_len(0:nrow(bc)*0.15,length.out = nrow(bc))
  return(bc)
})

cc<-bind_rows(cc,.id = "type")


# remove any "n" base
cc %>% dplyr::filter(group %in% c("a","g","c","t"))->cc

showtext_auto()
p<-ggplot()+geom_rect(data=cc,aes(xmin=xpos,xmax=xpos+0.15,ymin=0,ymax=1,show.legend = F,fill=group))+
  facet_wrap(vars(type),ncol = 1,strip.position = "top")+
  theme_void()+scale_fill_manual(values = c("#d11141","#00b159","#ffc425","#00aedb"))+
  theme(strip.background = element_blank(),
        strip.text.x = element_text(hjust = 0.068,size = 6),
        legend.position = "none",
        panel.spacing = unit(1,"lines")) #increase facet_warp space

pdf("~/Documents/DNAbarcode/p.pdf",width = 8,height = length(table(cc$type))/2)
plot(p)
dev.off()


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


