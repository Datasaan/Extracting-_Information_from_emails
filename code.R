cname="C:/Users/Vinod/Desktop/beck-s.tar/beck-s"      # Path to the beck-s directory
dirs=list.dirs(cname)                                  # Outputs paths to all the directories in beck-s
  
dft=list(last_name=c(),email=c(),first_name=c())
for(s in 2:length(dirs))
{
files=list.files(dirs[s],full.names=T)                # Outputs paths to all the files present in beck-s
for (k in 1:length(files))
  {
doh=scan(files[k],character(0))
#-----------------------------------------------------------------------------
# Extracts Sender's information
a=grep("^From:",doh)
b=grep("^X-From:",doh)
dft=NULL
m=1
dft$email[m]=doh[a[1]+1]
dft$first_name[m]=doh[b[1]+1]

dft$last_name[m]=doh[b[1]+2]
if(nchar(as.character(dft$last_name[m]))==1 )
{
  dft$last_name[m]=doh[b[1]+3]
}

#----------------------------------------------------------------
# Extracts information about all the people, the email was sent to
m=m+1
a=grep("To:",doh)
b=grep("^X-To:",doh)
l=1
f=b[1]
i=1

while(l==1)
{
  f=f+1
  
  dft$email[m]=doh[a[1]+i]
  dft$first_name[m]=doh[f]
  l=1*grepl(",",doh[a[1]+i])
  
  if(grepl(",",dft$first_name[m]) & l==1)
  {
    dft$last_name[m]= NA
  }
  else
  {
    f=f+1
    dft$last_name[m]=doh[f]
    if(l==1)
    {
      while( (grepl(",",dft$last_name[m])== F)  ) 
      {
        f=f+1
        dft$last_name[m]=doh[f]
      }
    }
    if(l==0)
    {
      if(grepl("X-cc:",doh[f+2])==T)
      {
        f=f+1
        dft$last_name[m]=doh[f]
      }
    }
  }
  m=m+1*l
  i=i+1
}
#---------------------------------------------------------------------------
# Extracts information about all the people, to whom a CC of the email was sent
m=m+1
a=grep("^Cc:",doh)
b=grep("^X-cc:",doh)
l=1
f=b[1]
i=1

while(l==1)
{
  f=f+1
  
  dft$email[m]=doh[a[1]+i]
  dft$first_name[m]=doh[f]
  l=1*grepl(",",doh[a[1]+i])
  
  if(grepl(",",dft$first_name[m]) & l==1)
  {
    dft$last_name[m]= NA
  }
  else
  {
    f=f+1
    dft$last_name[m]=doh[f]
    if(l==1)
    {
      while( (grepl(",",dft$last_name[m])== F)  ) 
      {
        f=f+1
        dft$last_name[m]=doh[f]
      }
    }
    if(l==0)
    {
      if(grepl("X-bcc:",doh[f+2])==T)
      {
        f=f+1
        dft$last_name[m]=doh[f]
      }
    }
  }
  m=m+1*l
  i=i+1
}
#------------------------------------------------------------------------------

#-----------------------------------------------------------------------------
# Cleans the data frame 


dft$last_name=sub(",","",dft$last_name)
dft$first_name=sub(",","",dft$first_name)
dft$email=sub(",","",dft$email)

dft=as.data.frame(dft)
dft=dft[grepl("\\.com|\\.edu|\\.net|@",dft$email),]
dft=dft[grepl("\\.com|\\.edu|\\.net|:",dft$first_name)==F,]
if(length(dft$email)>0)
{
for(u in 1:length(dft$email))
{
  if(grepl("@|:",dft$last_name[u]) | grepl("<",dft$last_name[u]) )
    dft$last_name[u]= NA
  
} 
#--------------------------------------------------------------------------------------
  
# Converts the data frame into JSON format as specified 
data1=list(lastname=c(),phone=c(),address=c(),company=c())
data1$lastname=dft$last_name
data1$phone=NA*(1:length(data1$lastname))
data1$company=NA*(1:length(data1$lastname))
data1$address=NA*(1:length(data1$lastname))
data1=as.data.frame(data1)
data2=data.frame(firstname=dft$first_name, email= dft$email)
data2$additionalDetails=data.frame(lastname=data1$lastname,phone=data1$phone,address=data1$address,company=data1$company)
json=toJSON(data2,pretty = T,na=c("string"))
write(json,file=files[k])
}
}
}