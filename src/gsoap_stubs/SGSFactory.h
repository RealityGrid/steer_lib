//gsoap sgsf schema namespace: http://vermont.mvc.mcc.ac.uk/SGS/factory
//gsoap apachesoap schema namespace: http://xml.apache.org/xml-soap
//gsoap soapenc schema namespace: http://schemas.xmlsoap.org/soap/encoding/

//gsoap sgsf service namespace: http://vermont.mvc.mcc.ac.uk/SGS/factory

//gsoap sgsf service location: http://foo.bar/
//gsoap sgsf service name: soapSGSFactory

/*start primitive data types
typedef char * xsd__string;*/

/*end primitive data types*/

struct sgsf__createServiceResponse {
	xsd__string  _createServiceReturn;
};

struct sgsf__destroyResponse {
        void *_;
};

struct sgsf__registerSelfResponse {
	xsd__string  _registerSelfReturn;
};

//gsoap sgsf service method-action: registerSelf ""
sgsf__registerSelf( xsd__string  in0, xsd__string  in1, struct sgsf__registerSelfResponse * out );
//gsoap sgsf service method-action: destroy ""
sgsf__destroy( void *_, struct sgsf__destroyResponse * out );
//gsoap sgsf service method-action: createService ""
sgsf__createService( xsd__string  in0, xsd__string  in1, struct sgsf__createServiceResponse * out );
