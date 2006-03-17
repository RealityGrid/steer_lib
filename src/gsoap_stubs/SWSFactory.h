//gsoap swsf schema namespace: http://www.sve.mvc.mcc.ac.uk/SWSFactory
//gsoap apachesoap schema namespace: http://xml.apache.org/xml-soap
//gsoap soapenc schema namespace: http://schemas.xmlsoap.org/soap/encoding/

//gsoap swsf service namespace: http://www.sve.mvc.mcc.ac.uk/SWSFactory

//gsoap swsf service location: http://some.where/
//gsoap swsf service name: soapSWSFactory

struct swsf__createSWSResourceResponse {
	struct epr wsa__EndpointReference;
};

//gsoap swsf service method-action: createService ""
swsf__createSWSResource( xsd__int  timeToLive, 
			 xsd__string  chkpointEPR,
			 xsd__string  passPhrase,
			 struct swsf__createSWSResourceResponse * out );
