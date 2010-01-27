//gsoap cptn schema namespace: http://www.RealityGrid.org/CheckPointTreeNode
//gsoap apachesoap schema namespace: http://xml.apache.org/xml-soap
//gsoap soapenc schema namespace: http://schemas.xmlsoap.org/soap/encoding/
//gsoap cptn service namespace: http://www.RealityGrid.org/CheckPointTreeNode
//gsoap cptn service location: http://foo.bar/
//gsoap cptn service name: soapCheckPointTreeNode

struct cptn__addNodeResponse {
  xsd__string  _addNodeReturn;
};

struct cptn__destroyResponse {
  void *_;
};

//gsoap rgt service method-action: destroy ""
cptn__Destroy( void *_,
	       struct cptn__destroyResponse * out );

//gsoap rgt service method-action: addNode ""
cptn__addNode( xsd__string  in0,
	       xsd__string  in1,
	       xsd__string  in2,
	       xsd__string  in3,
	       xsd__string  in4,
	       struct cptn__addNodeResponse * out );
