//gsoap rsg schema namespace: http://www.RealityGrid.org/regServiceGroup
//gsoap apachesoap schema namespace: http://xml.apache.org/xml-soap
//gsoap soapenc schema namespace: http://schemas.xmlsoap.org/soap/encoding/

//gsoap rsg service namespace: http://www.RealityGrid.org/regServiceGroup

//gsoap rsg service location: http://some.where/
//gsoap rsg service name: soapregServiceGroup

struct rsg__AddResponse {
  struct epr wsa__EndpointReference;
};

//gsoap rsg service method-action: Add ""
rsg__Add(XML _in, struct rsg__AddResponse *_out);
