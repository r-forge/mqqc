PIDhtmltable <- 
function(htmloutPath,htmlname = "MQQC_PID.html"){
  species   <- read.csv(paste(path.package("mqqc"),"data/MQQCspecies.csv",sep = "/"))
  if(length(species) == 1){
    species   <- read.csv2(paste(path.package("mqqc"),"data/MQQCspecies.csv",sep = "/"))
  }
  species$Fasta <- basename(as.character(species$Fasta))
  species$Xml <- basename(as.character(species$Xml))
  speciesHtml <- HtmlTable(species)


HtmlHeader <- paste("
<!-- $Id: example.html,v 1.4 2006/03/27 02:44:36 pat Exp $ -->
<!DOCTYPE HTML PUBLIC \"-//W3C//DTD HTML 4.01 Transitional//EN\" \"http://www.w3.org/TR/html4/loose.dtd\">
<html lang=\"en\">
<head>
<meta http-equiv=\"Content-Type\" content=\"text/html; charset=ISO-8859-1\">
<title>MQQC</title>

<body link=\"#cccccc\" alink=\"#0fff00\" vlink=\"#df5320\">

<div id=\"container\" style=\"width:100%\">

<div id=\"header\" style=\"background-color:#426787\">
<h1 style=\"margin-bottom:0;color:#ffffff\">MQQC - MaxQuant Quality Control ",packageVersion("mqqc"),"</h1></div>")

Tables <-  c(  "<div id=\"header\" style=\"background-color: #009ec2\"> <h3 style=\"margin-bottom:0;color:#ffffff\">  MQ Parameter IDs </h3> </div>", speciesHtml)


endHtml <- "</div>

</body>
</div>

</div>

<div id=\"footer\" style=\"background-color:#426787;height:5px;clear:both;text-align:center;\">
<a href = #> top </a>
</div>
</html>"
  
write(c(HtmlHeader,Tables, endHtml),htmlPath<- paste(htmloutPath,htmlname,sep = "/"))
#system(paste("open",htmlPath))
}
