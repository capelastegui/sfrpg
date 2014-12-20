require(plyr)
require(dplyr)


buildElement <- function(str, pre, post)
{laply(str, pre,post, .fun=function(s,pre,post){paste(pre,s,post,sep="")})}

buildElementApply <- function (df,pre,post, df.names=names(df))
{
  tmp<-ldply(df.names,df,pre,post,.fun=function(n,df,pre,post){buildElement(df[[n]],pre[[n]],post[[n]])})
  tmp<-laply(tmp,paste0, collapse="")
  paste0(pre$Body,tmp,post$Body, "\n",collapse="")
  #paste0(tmp,collapse="\r\n")
}
#TODO: change data frame from factors to characters
#tmp1<-buildElement(feat.raw.table$Name, feat.pre.table$Name, feat.post.table$Name)


buildTableApply <- function (df, df.names=names(df))
{
  tmp<-ldply(df.names,df,.fun=function(n,df){buildElement(df[[n]],"<td>","</td>")})
  tmp<-laply(tmp,paste0, collapse="")
  tmp<-paste0("<tr>",tmp,"</tr>", "\n",collapse="")
  tmp<-paste0("<table>",tmp,"</table>", "\n",collapse="")
  #paste0(tmp,collapse="\r\n")
}

