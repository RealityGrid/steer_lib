# Generate c stubs and headers for calling the methods on the SGS
# described in SGS.wsdl
JAVA = java
SOAP_CLASS_PATH = ${HOME}/gsoap-linux-2.2.3/wsdlcpp
SOAPCPP2 = ${HOME}/gsoap-linux-2.2.3/soapcpp2

${JAVA} -cp ${SOAP_CLASS_PATH} wsdlcpp -c SGS.wsdl
${SOAPCPP2} -c SGS.h

cp soapStub.h soapH.h soapSGS.nsmap ../include/.
cp soapC.c soapClient.c ../src/.
