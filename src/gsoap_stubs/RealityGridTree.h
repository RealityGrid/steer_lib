//gsoap rgt schema namespace: http://www.RealityGrid.org/RealityGridTree
/*//gsoap rgt schema namespace: http://www.RealityGrid.org/RealityGridTree/service*/
//gsoap apachesoap schema namespace: http://xml.apache.org/xml-soap
//gsoap soapenc schema namespace: http://schemas.xmlsoap.org/soap/encoding/

//gsoap rgt service namespace: http://www.RealityGrid.org/RealityGridTree

//gsoap rgt service location: http://foo.bar/
//gsoap rgt service name: soapRealityGridTree

/*start primitive data types*/
//typedef char * xsd__string;

/*end primitive data types*/

struct rgt__getParentNodeResponse {
	xsd__string  _getParentNodeReturn;
};

struct rgt__requestTerminationAfterResponse {
	xsd__string  _requestTerminationAfterReturn;
};

struct rgt__addNodeResponse {
	xsd__string  _addNodeReturn;
};

struct rgt__getCheckPointDataResponse {
	xsd__string  _getCheckPointDataReturn;
};

struct rgt__findServiceDataResponse {
	xsd__string  _findServiceDataReturn;
};

struct rgt__setCheckPointDataResponse {
	void *_;
};

struct rgt__getChildNodesResponse {
	xsd__string  _getChildNodesReturn;
};

struct rgt__requestTerminationBeforeResponse {
	xsd__string  _requestTerminationBeforeReturn;
};

struct rgt__getSteeringCommandsResponse {
	xsd__string  _getSteeringCommandsReturn;
};

struct rgt__getInputFileResponse {
	xsd__string  _getInputFileReturn;
};

struct rgt__destroyResponse {
        void *_;
};

//gsoap rgt service method-action: requestTerminationAfter ""
rgt__requestTerminationAfter( xsd__string  in0, struct rgt__requestTerminationAfterResponse * out );
//gsoap rgt service method-action: getChildNodes ""
rgt__getChildNodes( void *_, struct rgt__getChildNodesResponse * out );
//gsoap rgt service method-action: getInputFile ""
rgt__getInputFile( void *_, struct rgt__getInputFileResponse * out );
//gsoap rgt service method-action: findServiceData ""
rgt__findServiceData( xsd__string  in0, struct rgt__findServiceDataResponse * out );
//gsoap rgt service method-action: setCheckPointData ""
rgt__setCheckPointData( void *_, struct rgt__setCheckPointDataResponse * out );
//gsoap rgt service method-action: destroy ""
rgt__destroy( void *_, struct rgt__destroyResponse * out );
//gsoap rgt service method-action: getSteeringCommands ""
rgt__getSteeringCommands( void *_, struct rgt__getSteeringCommandsResponse * out );
//gsoap rgt service method-action: requestTerminationBefore ""
rgt__requestTerminationBefore( xsd__string  in0, struct rgt__requestTerminationBeforeResponse * out );
//gsoap rgt service method-action: getCheckPointData ""
rgt__getCheckPointData( void *_, struct rgt__getCheckPointDataResponse * out );
//gsoap rgt service method-action: getParentNode ""
rgt__getParentNode( void *_, struct rgt__getParentNodeResponse * out );
//gsoap rgt service method-action: addNode ""
rgt__addNode( xsd__string  in0, xsd__string  in1, xsd__string  in2, xsd__string  in3, xsd__string  in4, struct rgt__addNodeResponse * out );
