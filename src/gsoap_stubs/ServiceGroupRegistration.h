//gsoap sgr schema namespace: http://vermont.mvc.mcc.ac.uk/ServiceGroupRegistration
//gsoap apachesoap schema namespace: http://xml.apache.org/xml-soap
//gsoap soapenc schema namespace: http://schemas.xmlsoap.org/soap/encoding/

//gsoap sgr service namespace: http://vermont.mvc.mcc.ac.uk/ServiceGroupRegistration

//gsoap sgr service location: http://some.where/
//gsoap sgr service name: soapServiceGroupRegistration

/*start primitive data types*/
//typedef char * xsd__string;

/*end primitive data types*/

struct sgr__findServiceDataResponse {
	xsd__string  _findServiceDataReturn;
};

struct sgr__addResponse {
	struct ArrayOf_USCORE_xsd_USCORE_string * _addReturn;
};

struct ArrayOf_USCORE_xsd_USCORE_string {
	xsd__string * __ptr;
	int  __size;
	int  __offset;
};

struct sgr__removeResponse {
        void *_;
};

//gsoap sgr service method-action: remove ""
sgr__remove( xsd__string  in0, struct sgr__removeResponse * out );
//gsoap sgr service method-action: add ""
sgr__add( xsd__string  in0, xsd__string  in1, xsd__string  in2, struct sgr__addResponse * out );
//gsoap sgr service method-action: findServiceData ""
sgr__findServiceData( xsd__string  in0, struct sgr__findServiceDataResponse * out );
