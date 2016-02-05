#SGS functions

library(XML)
library(RCurl)

getSGSValue <- function(serie = "17632", data = "01/01/2003") {
	body <- paste('<soapenv:Envelope xmlns:xsi="http://www.w3.org/2001/XMLSchema-instance" xmlns:xsd="http://www.w3.org/2001/XMLSchema" xmlns:soapenv="http://schemas.xmlsoap.org/soap/envelope/" xmlns:pub="http://publico.ws.casosdeuso.sgs.pec.bcb.gov.br"><soapenv:Header/><soapenv:Body><pub:getValor soapenv:encodingStyle="http://schemas.xmlsoap.org/soap/encoding/"><in0 xsi:type="xsd:long">'
		, serie
		, '</in0><in1 xsi:type="xsd:string">'
		, data
		, '</in1></pub:getValor></soapenv:Body></soapenv:Envelope>'
		, sep = "")
	headerFields <- c(Accept = 'text/xml', Accept = 'multipart/*', 'Content-Type' = 'text/xml; charset=utf-8',SOAPAction='urn:#getValor')

	reader <- basicTextGatherer()
	ret <- curlPerform(url = "https://www3.bcb.gov.br/wssgs/services/FachadaWSSGS"
					, httpheader = headerFields
					, postfields = body
					, writefunction = reader$update)
	#reader$value()
	doc <- xmlInternalTreeParse(reader$value())
	rootNode <- xmlRoot(doc)
	return(as.numeric(xmlSApply(rootNode,xmlValue)[[1]]))
}

getSGSSeries <- function(series = "17632"
					, data.inic = "01/01/2003"
					, data.fim = "hoje"
					, charVars.rm = TRUE) {

	if (data.fim=="hoje") {data.fim <- format(Sys.Date(),"%d/%m/%Y")}

	xmlStrSeries <- character()
	for (serie in series) {
		xmlStrSeries <- paste(xmlStrSeries, '<oidSerie>', as.character(serie), '</oidSerie>', sep = "")
	}
	
	body <- paste('<soapenv:Envelope xmlns:xsi="http://www.w3.org/2001/XMLSchema-instance" xmlns:xsd="http://www.w3.org/2001/XMLSchema" xmlns:soapenv="http://schemas.xmlsoap.org/soap/envelope/" xmlns:pub="http://publico.ws.casosdeuso.sgs.pec.bcb.gov.br" xmlns:soapenc="http://schemas.xmlsoap.org/soap/encoding/"> <soapenv:Header/> <soapenv:Body>  <pub:getValoresSeriesXML soapenv:encodingStyle="http://schemas.xmlsoap.org/soap/encoding/">'
				, '<in0 xsi:type="def:ArrayOfflong" soapenc:arrayType="xsd:long[]" xmlns:def="http://schemas.xmlsoap.org/soap/encoding/">'
				, xmlStrSeries
				, '</in0><in1 xsi:type="xsd:string\">'
				, data.inic
				, '</in1> <in2 xsi:type=\"xsd:string\">'
				, data.fim
				, '</in2> </pub:getValoresSeriesXML> </soapenv:Body> </soapenv:Envelope>'
				, sep = "")

	headerFields = c(Accept = 'text/xml', Accept = 'multipart/*', 'Content-Type' = 'text/xml; charset=utf-8',SOAPAction='urn:#getValoresSeriesXML')

	reader = basicTextGatherer()
	ret <- curlPerform(url = "https://www3.bcb.gov.br/wssgs/services/FachadaWSSGS"
					, httpheader = headerFields 
					, postfields = body
					, writefunction = reader$update)
	doc <- xmlParse(reader$value())
	rootNode <- xmlRoot(doc)
	xmlStr <- xmlSApply(rootNode,xmlValue)[[1]]
	doc <- xmlInternalTreeParse(xmlStr)
	cleanup <- xpathApply(doc,"//SERIE", function(s) {
		id <- xmlGetAttr(s, "ID")
		s1 <- xmlSApply(s, function(x) xmlSApply(x, xmlValue))
		s1 <- t(s1)
		dimnames(s1) <- list(NULL, dimnames(s1)[[2]])
		df <- as.data.frame(s1, stringsAsFactors=FALSE)
		df$SERIE <- id
		df
  		})
  	df <- Reduce(rbind, cleanup)
  	
	library(stringr);
	if (str_count(df$DATA[1], "/") == 1) {
		df$data  <- as.Date(sapply(strsplit(df$DATA,  "/")
					, function(x) paste(c(x[2:1], 1), collapse="-"))
									, "%Y-%m-%d")
	} else if (str_count(df$DATA[1], "/") == 2) {
		df$data  <- as.Date(sapply(strsplit(df$DATA,  "/")
					, function(x) paste(c(x[3:1]), collapse="-"))
									, "%Y-%m-%d")
	} else if (str_count(df$DATA[1], "/") == 0) {
		df$data <- as.Date(sapply(df$DATA
					, function(x) paste(c(x, 1 , 1), collapse="-"))
									, "%Y-%m-%d")

	}
  	df$valor <- as.numeric(df$VALOR)
 	df$serie <- factor(df$SERIE)

  	if(charVars.rm){
    		df$BLOQUEADO <- NULL
    		df$SERIE <- NULL
    		df$DATA <- NULL
    		df$VALOR <- NULL
 	 }
  	return(df)
}

