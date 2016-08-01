#' generico
#' @description
#' \code{generico} plots direct ordination of species abundances.
#' @author
#' Created by Victor Lemes Landeiro. Updated 18-07-2008.
#' @details   
#'  Ported from: https://ppbio.inpa.gov.br/sites/default/files/Generico\%20R . 
#'  Tried and tested code used to develop package and github. Plots the relative abundance of species in samples
#'  ordered in relation to an environmental gradient. Species are plotted seperately. This ordering provides
#'  visual representation of the counts of different species that enables a clearly interpretable comparison of
#'  which species are found at the extremes of a gradient or occur through the range. 
#'  Most effective with relatively few species (i.e. < 30).
#'  
#'  The function is not general. It is not possible for users to ajust graphical parameters.
#'  This means that the graph presented is unlikely to be correct for publication.
#'  Function code is likely to need adaptation for specific use cases (e.g. more or less species).
#'  Used to teach students how to adapt code from existing functions. 
#' 
#' @param tabela Species data (object class: \code{data.frame}. Species in columns.
#' @param gradiente Vector with the environmental gradient for each row of the species table.
#' @param at Used to alter plotted position of the species names. Start with 1 and increase until obtain desired location.
#' @param grad  Character of the name of the environmental gradient to appear on the graph.
#' @param eixoY Name of y axis legend (character)
#' @param eixoX Name of x axis legend (character)
#'
#' @return Function \code{generico} plots bar charts of species abundances from sites 
#' ordered along an environmental gradient. 
#' @export
#' @importFrom "graphics" "axis" "barplot" "layout" "mtext" "par" "plot"
#' @references Examples of use:
#'   
#' Norris, D. et al. 2016. Too rare for non-timber resource harvest? 
#' Meso-scale composition and distribution of arborescent palms in an Amazonian sustainable-use forest.
#' Forest Ecology and Management 377: 182-191. http://dx.doi.org/10.1016/j.foreco.2016.07.008 .  
#' 
#' @examples
#' \dontrun{
#' # load species and environmental data
#' library(vegan) 
#' data(mite, mite.env)
#' dfsp <- mite[,1:10] #select first 10 mite species 
#' 
#' #plot 
#' generico(tabela = dfsp, gradiente = mite.env$WatrCont,
#' at=80,grad = "Water",eixoX="Site", eixoY="Count") 
#' }
generico<-function(tabela,gradiente,at,grad,eixoY,eixoX){
  tabela<-as.matrix(tabela)
  gradiente<-as.matrix(gradiente)
  media.pond<-colSums(tabela*gradiente[,1])/colSums(tabela)
# Ordenar parcelas de acordo com o gradiente
  sub.orden<-tabela[order(gradiente[,1],decreasing=F),]	
# colocar espécies ordenadas pela média ponderada
  sub.orde<-sub.orden[,order(media.pond,decreasing=T)] 

  dados.pa<-matrix(0,nrow(tabela),ncol(tabela))
  dados.pa[tabela>0]<-1
## para deletar possíveis colunas vazias (espécie que não ocorreu)
  ordenado<-sub.orde[,which(colSums(dados.pa)>0)] 
#Plot
  par(mfrow=c(ncol(ordenado)+1,1),mar=c(0,4,0.2,10),oma=c(3,1,1,6))
  layout(matrix(1:(ncol(ordenado)+1)),
         heights=c(3,rep(1,ncol(ordenado))))
  plot(sort(gradiente[,1]),axes=F,ylab="",
       mfg=c(21,1),lwd=10,las=2,lend="butt",
       frame.plot=F,xaxt="n",type="h",col="black",
       ylim=c(min(gradiente),max(gradiente)))
  axis(side=2,at=c(0,max(gradiente)),las=2)
  mtext(grad,3,outer=T,font=2,line=-5,padj=-15.5,las=2)
  for(i in 1:ncol(ordenado)){
    barplot(ordenado[,i],bty="l",axisnames=F,axes=FALSE,col="black")
    #axis(side=2,at=max(ordenado[,i]),las=2)
    mtext(colnames(ordenado)[i],3,line=-1.0,adj=0,at=at,cex=.8,font=3)
  }
  mtext(eixoX,1,outer=T,font=2,line=1.2)
  mtext(eixoY,2,font=2,outer=T,line=-2)
}


