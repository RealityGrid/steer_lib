//gsoap rgtf schema namespace: http://www.RealityGrid.org/factory
//gsoap apachesoap schema namespace: http://xml.apache.org/xml-soap
//gsoap soapenc schema namespace: http://schemas.xmlsoap.org/soap/encoding/

//gsoap rgtf service namespace: http://www.RealityGrid.org/factory

//gsoap rgtf service location: http://vermont.mvc.mcc.ac.uk:50000/Session/RealityGridTree/factory
//gsoap rgtf service name: soapRealityGridTreeFactory

/*start primitive data types*/
//typedef char * xsd__string;

/*end primitive data types*/

struct rgtf__requestTerminationBeforeResponse {
	xsd__string  _requestTerminationBeforeReturn;
};

struct rgtf__findServiceDataResponse {
	xsd__string  _findServiceDataReturn;
};

struct rgtf__findByHandleResponse {
	xsd__string  _findByHandleReturn;
};

struct rgtf__getActiveTreesResponse {
	xsd__string  _getActiveTreesReturn;
};

struct rgtf__createNewTreeResponse {
	xsd__string  _createNewTreeReturn;
};

struct rgtf__requestTerminationAfterResponse {
	xsd__string  _requestTerminationAfterReturn;
};

struct rgtf__destroyResponse {
       void *_;
};

//gsoap rgtf service method-action: createNewTree ""
rgtf__createNewTree( xsd__string  in0, xsd__string  in1, xsd__string  in2, xsd__string  in3, struct rgtf__createNewTreeResponse * out );
//gsoap rgtf service method-action: findByHandle ""
rgtf__findByHandle( xsd__string  in0, xsd__string  in1, struct rgtf__findByHandleResponse * out );
//gsoap rgtf service method-action: destroy ""
rgtf__destroy( void *_, struct rgtf__destroyResponse * out );
//gsoap rgtf service method-action: findServiceData ""
rgtf__findServiceData( xsd__string  in0, struct rgtf__findServiceDataResponse * out );
//gsoap rgtf service method-action: getActiveTrees ""
rgtf__getActiveTrees( void *_, struct rgtf__getActiveTreesResponse * out );
//gsoap rgtf service method-action: requestTerminationBefore ""
rgtf__requestTerminationBefore( xsd__string  in0, struct rgtf__requestTerminationBeforeResponse * out );
//gsoap rgtf service method-action: requestTerminationAfter ""
rgtf__requestTerminationAfter( xsd__string  in0, struct rgtf__requestTerminationAfterResponse * out );
