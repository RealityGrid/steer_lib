//gsoap wsse schema namespace: http://docs.oasis-open.org/wss/2004/01/oasis-200401-wss-wssecurity-secext-1.0.xsd
//gsoap wsu schema namespace: http://docs.oasis-open.org/wss/2004/01/oasis-200401-wss-wssecurity-utility-1.0.xsd
//gsoap wsa schema namespace: http://www.w3.org/2005/03/addressing

/*start primitive data types*/
typedef char * xsd__string;
typedef int xsd__int;
typedef char* XML;
/*end primitive data types*/

struct epr {
  xsd__string wsa__Address;
};

#import "SWS.h"
#import "SGS.h"
#import "SGSFactory.h"
#import "SWSFactory.h"
#import "ServiceGroupRegistration.h"
#import "RealityGridTreeFactory.h"
#import "RealityGridTree.h"
#import "regServiceGroup.h"
#import "CheckPointTree.h"
#import "CheckPointTreeNode.h"

struct stringWithAttr {
   char*  __item;
   @xsd__string Type;
};

struct UsernameToken {
  xsd__string wsse__Username;
  struct stringWithAttr wsse__Password;
  xsd__string wsse__Nonce;
  xsd__string wsu__Created;
};

struct Security {
  struct UsernameToken wsse__UsernameToken;
};

struct SOAP_ENV__Header
{ 
  xsd__string           wsa__To;
  struct Security wsse__Security;
};

