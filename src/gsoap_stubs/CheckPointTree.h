//gsoap cpt schema namespace: http://www.RealityGrid.org/CheckPointTree
//gsoap apachesoap schema namespace: http://xml.apache.org/xml-soap
//gsoap soapenc schema namespace: http://schemas.xmlsoap.org/soap/encoding/

//gsoap cpt service namespace: http://www.RealityGrid.org/CheckPointTree

//gsoap cpt service location: http://vermont.mvc.mcc.ac.uk:50000/Session/CheckPointTree
//gsoap cpt service name: soapCheckPointTree

struct cpt__findByHandleResponse {
	xsd__string  _findByHandleReturn;
};

struct cpt__getActiveTreesResponse {
	xsd__string  _getActiveTreesReturn;
};

struct cpt__createNewTreeResponse {
	xsd__string  _createNewTreeReturn;
};

struct cpt__destroyResponse {
       void *_;
};

//gsoap cpt service method-action: createNewTree ""
cpt__createNewTree( xsd__string  in0, 
		    struct cpt__createNewTreeResponse * out );
//gsoap cpt service method-action: findByHandle ""
cpt__findByHandle( xsd__string  in0, xsd__string  in1, 
		   struct cpt__findByHandleResponse * out );
//gsoap cpt service method-action: destroy ""
cpt__Destroy( void *_, 
	      struct cpt__destroyResponse * out );
//gsoap cpt service method-action: getActiveTrees ""
cpt__getActiveTrees( void *_, 
		     struct cpt__getActiveTreesResponse * out );


