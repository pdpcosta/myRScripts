setwd("~/Documents/Doutorado/TesteEmocoesOut2014/Analysis/")
data<-read.csv2('datafile_example.csv',header=TRUE,sep=',')
data$result<-ifelse(data$Stimulus.==data$VoteVP2.,1,0)
old_par<-par()
emnames=c("Happy For",
          "Joy",
          "Hope",
          "Satisfaction",
          "Relief",
          "Pride",
          "Gratification",
          "Gratitude",
          "Admiration",
          "Love",
          "Pity",
          "Sadness",
          "Fear",
          "Resentment",
          "Fears Confirmed",
          "Shame",
          "Reproach",
          "Remorse",
          "Gloating",
          "Disapointment",
          "Disgust",
          "Anger",
          "NOTA")

###########################################################################
# Analysis of the Perceptual Evaluation of Emotions Valence Results
###########################################################################
valence_video<-table(data[data$Type_Name=='video',]$Stimulus_Name,
                      data[data$Type_Name=='video',]$VoteVP1_Name)
valence_animation<- table(data[data$Type_Name=='animation',]$Stimulus_Name,
                     data[data$Type_Name=='animation',]$VoteVP1_Name)

write.table(valence_video,"valence_video.csv",sep=",")
write.table(valence_animation,"valence_animation.csv",sep=",")

#
# Fisher's exact test  - Valence Voting Option versus Type of Stimulus
#
valence_pvalue<-c()
for (i in levels(data$Stimulus_Name)){
  # Creates contigency table
  vtable<-table(data[data$Stimulus_Name==i,]$VoteVP1.,data[data$Stimulus_Name==i,]$Type_Name)
  # Computes the p-value of Fisher's exact test
  valence_pvalue<-rbind(valence_pvalue,fisher.test(vtable)$p.value)
}
valence<-data.frame(levels(data$Stimulus_Name),round(valence_pvalue,digits=4))
write.table(valence,"valence_fisher_pvalues.csv",sep=",",row.names=FALSE)

#
# Print example of contigency table for anger emotion
#
i='anger'
vtable<-table(data[data$Stimulus_Name==i,]$VoteVP1.,data[data$Stimulus_Name==i,]$Type_Name)
print("Example of Contigency Table - Anger")
print (vtable)

#
# Print example of contigency table for relief emotion
#
i='relief'
vtable<-table(data[data$Stimulus_Name==i,]$VoteVP1.,data[data$Stimulus_Name==i,]$Type_Name)
print("Example of Contigency Table - Relief")
print (vtable)

#
# Wilcoxon-Mann-Whitney test - Considering the valence of Emotions an
# ordinal variable
#
valence_pvalue_wilcox<-c()
for (i in levels(data$Stimulus_Name)){
  valence_pvalue_wilcox<-rbind(valence_pvalue_wilcox,
                               wilcox.test(data[data$Stimulus_Name==i & data$Type_Name=="video",]$VoteVP1.,
                                           data[data$Stimulus_Name==i & data$Type_Name=="animation",]$VoteVP1.)$p.value)
}
valencewilcox<-data.frame(levels(data$Stimulus_Name),round(valence_pvalue_wilcox,digits=4))
write.table(valencewilcox,"valence_wilcox_pvalues.csv",sep=",",row.names=FALSE)

###########################################################################
# Analysis of the Emotion Recognition Test
###########################################################################
correctvideo<-c()
correctanimation<-c()
s<-c()

for (i in levels(data$Subject)){
  correctvideo<-rbind(correctvideo,sum(data$result[data$Subject==i & data$Type_Name=="video"]))
  correctanimation<-rbind(correctanimation,sum(data$result[data$Subject==i & data$Type_Name=="animation"]))
  s<-rbind(s,i)
}

#
# Generate boxplot of correct answers
#
correctanswers<-data.frame(correctvideo,correctanimation,row.names=levels(data$Subject))
old_par<-par()
par(mar=c(4.5, 5, 2, 1)) # Change the size of the boxplot margins c(bottom, left, top, right)
par(cex.axis=1.3)
par(cex.lab=1.3)
boxplot((correctanswers/22)*100,
        ylab="Percentage of Correct Answers (%)",
        ylim=c(0,50),
        names=c("Real Video","Facial Animation"),
        col=c("orange","yellow"),
        boxwex=0.5)


#
# Print the summary statistics for both real video and facial animation
#
print(summary(correctvideo))
print(summary(correctanimation))



#
# Print the Shapiro-Test for normality p-values
#
print(round(shapiro.test(correctvideo)$p.value,digits=4))
print(round(shapiro.test(correctanimation)$p.value,digits=4))

#
# Print the resulting p-value of WMW test
#
print(round(wilcox.test(correctvideo,correctanimation)$p.value,digits=4))

#
# Contingency tables of Emotion Recognition --------------------------------
#

recognition_pvalue<-c()
# # Exact Fisher Test Recognition Rates per Emotion: Animation x Video
# valence_pvalue<-c()
for (i in levels(data$Stimulus_Name)){
  vtable<-table(data[data$Stimulus_Name==i,]$VoteVP2_Name,data[data$Stimulus_Name==i,]$Type_Name)
  recognition_pvalue<-rbind(recognition_pvalue,fisher.test(vtable)$p.value)
}
recognition<-data.frame(levels(data$Stimulus_Name),round(recognition_pvalue,digits=4))
write.table(recognition,"recognition.csv",sep=",",row.names=FALSE)

#
# Voted Emotions Barplots   ------------------------------------------
#

t<-table(data$VoteVP2.[data$Type_Name=="video"],data$Stimulus.[data$Type_Name=="video"])
t<-as.data.frame.array(t,row.names=emnames)
names(t)<-emnames[1:22]

a<-table(data$VoteVP2.[data$Type_Name=="animation"],data$Stimulus.[data$Type_Name=="animation"])
a<-as.data.frame.array(a,row.names=emnames)
names(a)<-emnames[1:22]

for (i in names(a)){  
  par(mfrow=c(2,1)) # 2 rows, 1 column
    
  s<-sort(t[[i]],decreasing=TRUE,index.return=TRUE)
  
  aux=0
  j=0
  while(aux<sum(t[[i]])){
    j=j+1
    aux=aux+s$x[j]
  }
  s$x<-s$x/sum(t[[i]])*100
  bp<-barplot(s$x[1:j],
              main=paste(i," - Real Video"),
              axes=FALSE,
              axisnames=FALSE, 
              ylab="Votes (%)",
              col=rep("orange",length(s$x[1:j])),
              ylim=c(0,100))
  # The text command below draw the xaxis labels inclinated
  # http://jnlnet.wordpress.com/2009/05/20/x-axis-labels-on-a-45-degree-angle-using-r/
  text(bp, par("usr")[3], labels = emnames[s$ix[1:j]], srt = 45, adj = c(1.1,1.1), xpd = TRUE, cex=.9)
  axis(2)
  
  
  s<-sort(a[[i]],decreasing=TRUE,index.return=TRUE)
  #s$x<-round((s$x/sum(a[[i]]))*100,digits=1)
  aux=0
  j=0
  #while(aux<sum(a[[i]])){
  while(aux<sum(a[[i]])){
    j=j+1
    aux=aux+s$x[j]
    print (aux)
  }
  s$x<-s$x/sum(a[[i]])*100  
  bp<-barplot(s$x[1:j],
              main=paste(i," - Facial Animation"),
              axes=FALSE,
              axisnames=FALSE, 
              ylab="Votes (%)",
              col=rep("yellow",length(s$x[1:j])),
              ylim=c(0,100))
  # The text command below draw the xaxis labels inclinated
  # http://jnlnet.wordpress.com/2009/05/20/x-axis-labels-on-a-45-degree-angle-using-r/
  text(bp, par("usr")[3], labels = emnames[s$ix[1:j]], srt = 45, adj = c(1.1,1.1), xpd = TRUE, cex=.9)
  axis(2)
  
}

