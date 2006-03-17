//gsoap sgs schema namespace: http://vermont.mvc.mcc.ac.uk/SGS
//gsoap apachesoap schema namespace: http://xml.apache.org/xml-soap
//gsoap soapenc schema namespace: http://schemas.xmlsoap.org/soap/encoding/

//gsoap sgs service namespace: http://vermont.mvc.mcc.ac.uk/SGS

//gsoap sgs service location: http://foo.bar/
//gsoap sgs service name: soapSGS

/*start primitive data types
typedef char * xsd__string;
typedef int xsd__int;
*/
/*end primitive data types*/

struct sgs__GetParamLogResponse {
	xsd__string  _GetParamLogReturn;
};

struct sgs__DetachResponse {
	xsd__string  _DetachReturn;
};

struct sgs__AppRecordChkpointResponse {
	xsd__string  _AppRecordChkpointReturn;
};

struct sgs__PutStatusResponse {
	xsd__string  _PutStatusReturn;
};

struct sgs__setServiceDataResponse {
	xsd__string  _setServiceDataReturn;
};

struct sgs__PauseResponse {
	xsd__string  _PauseReturn;
};

struct sgs__findServiceDataResponse {
	xsd__string  _findServiceDataReturn;
};

struct sgs__RestartResponse {
	xsd__string  _RestartReturn;
};

struct sgs__PutControlResponse {
	xsd__string  _PutControlReturn;
};

struct sgs__ResumeResponse {
	xsd__string  _ResumeReturn;
};

struct sgs__AttachResponse {
	xsd__string  _AttachReturn;
};

struct sgs__GetNotificationsResponse {
	xsd__string  _GetNotificationsReturn;
};

struct sgs__StopResponse {
	xsd__string  _StopReturn;
};

struct sgs__GetControlResponse {
	xsd__string  _GetControlReturn;
};

struct sgs__GetStatusResponse {
	xsd__string  _GetStatusReturn;
};

struct sgs__AppStopResponse {
	xsd__string  _AppStopReturn;
};

struct sgs__destroyResponse {
        void *_;
};

struct sgs__ClearStatusMsgQueueResponse {
	xsd__string  _ClearStatusMsgQueueReturn;
};

struct sgs__AppPutLogResponse {
	xsd__string  _AppPutLogReturn;
};

struct sgs__GetNthDataSourceResponse {
	xsd__string  _GetNthDataSourceReturn;
};

struct sgs__AppStartResponse {
	xsd__string  _AppStartReturn;
};

struct sgs__AppDetachResponse {
	xsd__string  _AppDetachReturn;
};

//gsoap sgs service method-action: Attach ""
sgs__Attach( void *_, struct sgs__AttachResponse * out );
//gsoap sgs service method-action: GetStatus ""
sgs__GetStatus( void *_, struct sgs__GetStatusResponse * out );
//gsoap sgs service method-action: Stop ""
sgs__Stop( void *_, struct sgs__StopResponse * out );
//gsoap sgs service method-action: ClearStatusMsgQueue ""
sgs__ClearStatusMsgQueue( void *_, struct sgs__ClearStatusMsgQueueResponse * out );
//gsoap sgs service method-action: Detach ""
sgs__Detach( void *_, struct sgs__DetachResponse * out );
//gsoap sgs service method-action: Resume ""
sgs__Resume( void *_, struct sgs__ResumeResponse * out );
//gsoap sgs service method-action: AppRecordChkpoint ""
sgs__AppRecordChkpoint( xsd__string  in0, xsd__string  in1, struct sgs__AppRecordChkpointResponse * out );
//gsoap sgs service method-action: PutStatus ""
sgs__PutStatus( xsd__string  in0, struct sgs__PutStatusResponse * out );
//gsoap sgs service method-action: AppStart ""
sgs__AppStart( void *_, struct sgs__AppStartResponse * out );
//gsoap sgs service method-action: GetParamLog ""
sgs__GetParamLog( xsd__int  in0, struct sgs__GetParamLogResponse * out );
//gsoap sgs service method-action: setServiceData ""
sgs__setServiceData( xsd__string  in0, struct sgs__setServiceDataResponse * out );
//gsoap sgs service method-action: AppPutLog ""
sgs__AppPutLog( xsd__string  in0, struct sgs__AppPutLogResponse * out );
//gsoap sgs service method-action: AppDetach ""
sgs__AppDetach( void *_, struct sgs__AppDetachResponse * out );
//gsoap sgs service method-action: Pause ""
sgs__Pause( void *_, struct sgs__PauseResponse * out );
//gsoap sgs service method-action: PutControl ""
sgs__PutControl( xsd__string  in0, struct sgs__PutControlResponse * out );
//gsoap sgs service method-action: Restart ""
sgs__Restart( xsd__string  in0, struct sgs__RestartResponse * out );
//gsoap sgs service method-action: GetNthDataSource ""
sgs__GetNthDataSource( xsd__int  in0, struct sgs__GetNthDataSourceResponse * out );
//gsoap sgs service method-action: findServiceData ""
sgs__findServiceData( xsd__string  in0, struct sgs__findServiceDataResponse * out );
//gsoap sgs service method-action: destroy ""
sgs__destroy( void *_, struct sgs__destroyResponse * out );
//gsoap sgs service method-action: GetControl ""
sgs__GetControl( void *_, struct sgs__GetControlResponse * out );
//gsoap sgs service method-action: GetNotifications ""
sgs__GetNotifications( void *_, struct sgs__GetNotificationsResponse * out );
//gsoap sgs service method-action: AppStop ""
sgs__AppStop( void *_, struct sgs__AppStopResponse * out );
