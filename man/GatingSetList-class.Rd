\name{GatingSetList-class}
\alias{GatingSetList-class}
\title{Class \code{"GatingSetList"}}
\description{
  A list of of \code{GatingSet} objects. This class exists
  for method dispatching.}
}
\details{
  Objects store a collection of GatingSets,which usually
  has the same gating trees and markers.  Most GatingSets
  methods can be applied to GatingSetList.
}
\examples{
\dontrun{
    #load several GatingSets from disk
   gs_list<-lapply(list.files("~/rglab/workspace/flowIncubator/output/gs_toMerge",full=T),function(this_folder){
          load_gs(this_folder)
        })

   #gs_list is a list
    gs_groups <- merge(gs_list)
    #returns a list of GatingSetList objects
    gslist2 <- gs_groups[[2]]
    #gslist2 is a GatingSetList that contains multiple GatingSets and they share the same gating and data structure
    gslist2
    class(gslist2)
    getSamples(gslist2)

    #reference a GatingSet by numeric index
    gslist2[[1]]
    #reference a GatingSet by character index
    gslist2[["30104.fcs"]]

    #loop through all GatingSets within GatingSetList
    lapply(gslist2,getSamples)

    #subset a GatingSetList by [
    getSamples(gslist2[c(4,1)])
    getSamples(gslist2[c(1,4)])
    gslist2[c("30104.fcs")]

    #get flow data from it
    getData(gslist2)
    #get gated flow data from a particular popoulation (by numeric or character index)
    getData(gslist2,4)

    #extract the gates associated with one popoulation
    getGate(gslist2,"3+")
    getGate(gslist2,5)

    #extract the pheno data
    pData(gslist2[3:1])
    #modify the pheno data
    pd <- pData(gslist2)
    pd$id <- 1:nrow(pd)
    pData(gslist2) <- pd
    pData(gslist2[3:2])

    #plot the gate
    plotGate(gslist2[1:2],5,smooth=T)
    plotGate_labkey(gslist2[3:4],4,x="<APC Cy7-A>",y="<PE Tx RD-A>",smooth=T)

    #remove cerntain gates by loop through GatingSets
    getNodes(gslist2[[1]])
    lapply(gslist2,function(gs)Rm("Excl",gs))

    #extract the stats
    getPopStats(gslist2)
    #extract statistics by using getQAStats defined in QUALIFIER package
    res<-getQAStats(gslist2[c(4,2)],isMFI=F,isSpike=F,nslaves=1)

    #archive the GatingSetList
    save_gslist(gslist2, path ="~/rglab/workspace/flowIncubator/output/gslist",overwrite=T)
    gslist2 <- load_gslist(path ="~/rglab/workspace/flowIncubator/output/gslist")

    #convert GatingSetList into one GatingSet by rbind2
    gs_merged2 <- rbind2(gslist2,ncdfFile=path.expand(tempfile(tmpdir="~/rglab/workspace/flowIncubator/output/",fileext=".nc")))
    gs_merged2
}
\seealso{
  \code{\linkS4class{GatingSet}}
  \code{\linkS4class{GatingHierarchy}}
}

