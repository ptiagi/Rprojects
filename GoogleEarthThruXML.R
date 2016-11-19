### This project will use Google Earth for data display. 

require("XML")


### Part 1.  Create the data frame

### Load the data frame called LatLon.

load("Latlon.rda")
dim(LatLon)
names(LatLon)

### Download the gzipped XML factbook document from
### http://jmatchparser.sourceforge.net/factbook/
### and create an XML "tree" in R 

factbook <- xmlParse("/Users/Dexter/Desktop/DataScience_Assignment/HW8_490IDS_Fall_2016/factbook.xml")
factbook_root <- xmlRoot(factbook)

getNodeSet(factbook_root, '//field[@name="Infant mortality rate"]')
getNodeSet(factbook_root, '//field[@name="Population"]')

### Use XPath to extract the infant mortality and the CIA country codes from the XML tree

infant <- xpathSApply(factbook, "//field[@name='Infant mortality rate']/rank", xmlGetAttr, "number")
country <- toupper(xpathSApply(factbook, "//field[@name='Infant mortality rate']/rank", xmlGetAttr, "country"))

### Create a data frame called IM using this XML file.
### The data frame should have 2 columns: for Infant Mortality and CIA.Codes.

IM <- data.frame("Infant Mortality" = infant, "CIA.Codes" = country)
head(IM)


### Extract the country populations from the same XML document
### Create a data frame called Pop using these data.
### This data frame should also have 2 columns, for Population and CIA.Codes.

population <- xpathSApply(factbook, "//field[@name='Population']/rank", xmlGetAttr, "number")
country <- toupper(xpathSApply(factbook, "//field[@name='Population']/rank", xmlGetAttr, "country"))

Pop <- data.frame("Population" = population, "CIA.Codes" = country)
dim(Pop)

### Merge the two data frames to create a data frame called IMPop with 3 columns:
### IM, Pop, and CIA.Codes

IMPop <- merge(IM, Pop, by="CIA.Codes")
head(IMPop)

### Now merge IMPop with LatLon (from newLatLon.rda) to create a data frame called AllData that has 6 columns
### for Latitude, Longitude, CIA.Codes, Country Name, Population, and Infant Mortality
### (please check lat,long are not reversed in the file)

AllData <- merge(IMPop, LatLon, by="CIA.Codes")

### Part 2.  Create a KML document
### Make the KML document described in HW8.pdf.  It should have the basic
### structure shown in that document.  You can use the addPlacemark function below to make
### the Placemark nodes, you just need to complete the line for the Point node and
### figure out how to use the function.

makeBaseDocument = function(){
  ### This code creates the template KML document 
  doc = newXMLDoc()
  root = newXMLNode("kml", namespaceDefinitions = c(xmlns = "http://www.opengis.net/kml/2.2"), doc = doc)
  document = newXMLNode("Document", parent=root)
  newXMLNode("Name","Country Facts", parent = document) 
  newXMLNode("Description", "Infant Mortality", parent = document)
  lookat = newXMLNode("LookAt", parent = document)
  newXMLNode("longitude", "-121", parent = lookat)
  newXMLNode("latitude", "43", parent = lookat)
  newXMLNode("altitude", "4100000", parent = lookat)
  newXMLNode("title", "0", parent = lookat)
  newXMLNode("heading", "0", parent = lookat)
  newXMLNode("altitudeMode", "absolute", parent = lookat)
  folder = newXMLNode("Folder", parent = document)
  newXMLNode("Name", "CIA Fact Book", parent = folder)
  return(doc)
}

kmldoc <- makeBaseDocument()

addPlacemark = function(lat, lon, ctryCode, ctryName, pop, infM, parent, 
                        inf1, pop1, style = FALSE)
{
  p = newXMLNode("Folder", parent = parent)
  newXMLNode("Name", "Country Facts",parent = p)
  {
    for (i in 1:length(ctryName))
    {
      pm = newXMLNode("Placemark", newXMLNode("name", ctryName[i]), attrs = c(id = ctryCode[i]), parent = p)
      newXMLNode("description", paste(ctryName[i], "\n Population: ", pop[i], "\n Infant Mortality: ", infM[i], sep =""), parent = pm)
      newXMLNode("Point", newXMLNode("coordinates", paste(lon[i], lat[i], 0 , sep=",")), parent=pm)
      
      ### You need to fill in the code for making the Point node above, including coordinates.
      ### The line below won't work until you've run the code for the next section to set up
      ### the styles.
      if(style) newXMLNode("styleUrl", paste("#YOR", inf1[i], "-", pop1[i], sep = ''), parent = pm)   
      
    }
  }
}

addPlacemark(lon = AllData$Longitude, lat=AllData$Latitude, ctryCode = AllData$CIA.Codes, ctryName = AllData$Country.Name,
             pop = AllData$Population, infM = AllData$Infant.Mortality, parent = getNodeSet(kmldoc,"/kml/Document"), inf1 = AllData$Infant.Mortality, 
             pop1 = AllData$Population)

### Save your KML document here, call it Part2.kml, and open it in Google Earth.
### (You will need to install Google Earth.)  
### It should have pushpins for all the countries.

saveXML(kmldoc, "Part2.kml")

### Part 3.  Add Style to your KML
### Now you are going to make the visualizatiion a bit fancier.  Pretty much all the code is given to you
### below to create style elements that are to be placed near the top of the document.
### These , you just need to figure out what it all does.

### Start fresh with a new KML document, by calling makeBaseDocument()

doc2 = makeBaseDocument()

### The following code is an example of how to create cut points for 
### different categories of infant mortality and population size.
### Figure out what cut points you want to use and modify the code to create these 
### categories.

infCut = cut(as.numeric(AllData[,2]), breaks = c(0, 10, 25, 50, 75, 200))
infCut = as.numeric(infCut)
popCut = cut(log(as.numeric(AllData[,3])), breaks = 5)
popCut = as.numeric(popCut)


### Now figure out how to add styles and placemarks to doc2
### You'll want to use the addPlacemark function with style = TRUE

### Below is code to make style nodes. 
### You should not need to do much to it.

### You do want to figure out what scales to you for the sizes of your circles

scales_new = c(0.5, 1, 3, 5, 10)

addStyle = function(col1, pop1, parent, urlBase, scales = scales_new)
{
  st = newXMLNode("Style", attrs = c("id" = paste("YOR", col1, "-", pop1, sep="")), parent = parent)
  newXMLNode("IconStyle", 
             newXMLNode("scale", scales[pop1]), 
             newXMLNode("Icon", paste(urlBase, "color_label_circle_", c("blue","green","orange","red","yellow")[col1], ".png", sep ="")), parent = st)
}

for (k in 1:5)
{
  for (j in 1:5)
  {
    addStyle(j, k, parent = getNodeSet(doc2,"/kml/Document"), 'http://web.stanford.edu/~vcs/StatData/circles/')
  }
}

### You will need to figure out what order to call addStyle() and addPlacemark()
### so that the tree is built properly. You may need to adjust the code to call the png files

addPlacemark(lon = AllData$Longitude, lat=AllData$Latitude, ctryCode = AllData$CIA.Codes, ctryName = AllData$Country.Name,
             pop = AllData$Population, infM = AllData$Infant.Mortality, parent = getNodeSet(doc2,"/kml/Document"), inf1 = infCut, 
             pop1 = popCut, style = TRUE)

### Finally, save your KML document, call it Part3.kml and open it in Google Earth to 
### verify that it works.  For this assignment, you only need to submit your code, 
### nothing else.  You can assume that the grader has already loaded HW8.rda.

saveXML(doc2, "Part3.kml")
