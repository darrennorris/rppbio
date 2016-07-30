# generico
#
# ### Created by Victor Lemes Landeiro ######
# ### Updated 18-07-2008  ###
#
# Ported from https://ppbio.inpa.gov.br/sites/default/files/Generico%20R.doc
#
# Tried and tested code used to setup package and github

generico<-function(tabela,gradiente,at,grad,eixoY,eixoX){
  tabela<-as.matrix(tabela)
  gradiente<-as.matrix(gradiente)
  media.pond<-colSums(tabela*gradiente[,1])/colSums(tabela)
  sub.orden<-tabela[order(gradiente[,1],decreasing=F),]	# Ordenar parcelas de acordo com o gradiente
  sub.orde<-sub.orden[,order(media.pond,decreasing=T)] # colocar espécies ordenadas pela média ponderada

  dados.pa<-matrix(0,nrow(tabela),ncol(tabela))
  dados.pa[tabela>0]<-1

  ordenado<-sub.orde[,which(colSums(dados.pa)>0)] ## para deletar possíveis colunas vazias (espécie que não ocorreu)

  par(mfrow=c(ncol(ordenado)+1,1),mar=c(0,4,0.2,10),oma=c(3,1,1,6))
  layout(matrix(1:(ncol(ordenado)+1)),heights=c(3,rep(1,ncol(ordenado))))
  plot(sort(gradiente[,1]),axes=F,ylab="",mfg=c(21,1),lwd=10,las=2,lend="butt",frame.plot=F,xaxt="n",type="h",col="black",ylim=c(min(gradiente),max(gradiente)))
  axis(side=2,at=c(0,max(gradiente)),las=2)
  mtext(grad,4,outer=T,font=2,line=-10,padj=-18.5,las=2)
  for(i in 1:ncol(ordenado)){
    barplot(ordenado[,i],bty="l",axisnames=F,axes=FALSE,col="black")
    #axis(side=2,at=max(ordenado[,i]),las=2)
    mtext(colnames(ordenado)[i],3,line=-1.0,adj=0,at=at,cex=.8,font=3)
  }
  mtext(eixoX,1,outer=T,font=2,line=1.2)
  mtext(eixoY,2,font=2,outer=T,line=-2)
}
