/* soapStub.h
   Generated by gSOAP 2.2.3b from SGS.h
   Copyright (C) 2001-2003 Genivia inc.
   All Rights Reserved.
*/
#ifndef soapStub_H
#define soapStub_H
#ifdef __cplusplus
extern "C" {
#endif

/* Types With Custom (De)serializers: */

/* Enumerations */

/* Classes and Structs */

#ifndef _SOAP_tns__GetStatusResponse
#define _SOAP_tns__GetStatusResponse
struct tns__GetStatusResponse
{
	char *_result;
};
#endif

#ifndef _SOAP_tns__AppStartResponse
#define _SOAP_tns__AppStartResponse
struct tns__AppStartResponse
{
	char *_result;
};
#endif

#ifndef _SOAP_tns__StopResponse
#define _SOAP_tns__StopResponse
struct tns__StopResponse
{
	char *_result;
};
#endif

#ifndef _SOAP_tns__SetServiceDataResponse
#define _SOAP_tns__SetServiceDataResponse
struct tns__SetServiceDataResponse
{
	char *_result;
};
#endif

#ifndef _SOAP_tns__AppStopResponse
#define _SOAP_tns__AppStopResponse
struct tns__AppStopResponse
{
	char *_result;
};
#endif

#ifndef _SOAP_tns__DetachResponse
#define _SOAP_tns__DetachResponse
struct tns__DetachResponse
{
	char *_result;
};
#endif

#ifndef _SOAP_tns__FindServiceDataResponse
#define _SOAP_tns__FindServiceDataResponse
struct tns__FindServiceDataResponse
{
	char *_result;
};
#endif

#ifndef _SOAP_tns__ResumeResponse
#define _SOAP_tns__ResumeResponse
struct tns__ResumeResponse
{
	char *_result;
};
#endif

#ifndef _SOAP_tns__AppDetachResponse
#define _SOAP_tns__AppDetachResponse
struct tns__AppDetachResponse
{
	char *_result;
};
#endif

#ifndef _SOAP_tns__PutControlResponse
#define _SOAP_tns__PutControlResponse
struct tns__PutControlResponse
{
	char *_result;
};
#endif

#ifndef _SOAP_tns__PutStatusResponse
#define _SOAP_tns__PutStatusResponse
struct tns__PutStatusResponse
{
	char *_result;
};
#endif

#ifndef _SOAP_tns__AttachResponse
#define _SOAP_tns__AttachResponse
struct tns__AttachResponse
{
	char *_result;
};
#endif

#ifndef _SOAP_tns__SetTerminationTimeResponse
#define _SOAP_tns__SetTerminationTimeResponse
struct tns__SetTerminationTimeResponse
{
	char *_result;
};
#endif

#ifndef _SOAP_tns__PauseResponse
#define _SOAP_tns__PauseResponse
struct tns__PauseResponse
{
	char *_result;
};
#endif

#ifndef _SOAP_tns__GetNotificationsResponse
#define _SOAP_tns__GetNotificationsResponse
struct tns__GetNotificationsResponse
{
	char *_result;
};
#endif

#ifndef _SOAP_tns__DestroyResponse
#define _SOAP_tns__DestroyResponse
struct tns__DestroyResponse
{
  void *rubbish; /* ARPDBG */
};
#endif

#ifndef _SOAP_tns__GetControlResponse
#define _SOAP_tns__GetControlResponse
struct tns__GetControlResponse
{
	char *_result;
};
#endif

#ifndef _SOAP_tns__GetNotifications
#define _SOAP_tns__GetNotifications
struct tns__GetNotifications
{
  void *rubbish; /* ARPDBG */
};
#endif

#ifndef _SOAP_tns__AppDetach
#define _SOAP_tns__AppDetach
struct tns__AppDetach
{
  void *rubbish; /* ARPDBG */
};
#endif

#ifndef _SOAP_tns__Detach
#define _SOAP_tns__Detach
struct tns__Detach
{
  void *rubbish; /* ARPDBG */
};
#endif

#ifndef _SOAP_tns__AppStop
#define _SOAP_tns__AppStop
struct tns__AppStop
{
  void *rubbish; /* ARPDBG */
};
#endif

#ifndef _SOAP_tns__SetServiceData
#define _SOAP_tns__SetServiceData
struct tns__SetServiceData
{
	char *input;
	char *sde_USCORE_value;
};
#endif

#ifndef _SOAP_tns__Pause
#define _SOAP_tns__Pause
struct tns__Pause
{
  void *rubbish; /* ARPDBG */
};
#endif

#ifndef _SOAP_tns__Stop
#define _SOAP_tns__Stop
struct tns__Stop
{
  void *rubbish; /* ARPDBG */
};
#endif

#ifndef _SOAP_tns__PutStatus
#define _SOAP_tns__PutStatus
struct tns__PutStatus
{
	char *input;
};
#endif

#ifndef _SOAP_tns__GetStatus
#define _SOAP_tns__GetStatus
struct tns__GetStatus
{
  void *rubbish; /* ARPDBG */
};
#endif

#ifndef _SOAP_tns__Resume
#define _SOAP_tns__Resume
struct tns__Resume
{
  void *rubbish; /* ARPDBG */
};
#endif

#ifndef _SOAP_tns__PutControl
#define _SOAP_tns__PutControl
struct tns__PutControl
{
	char *input;
};
#endif

#ifndef _SOAP_tns__Destroy
#define _SOAP_tns__Destroy
struct tns__Destroy
{
  void *rubbish; /* ARPDBG */
};
#endif

#ifndef _SOAP_tns__AppStart
#define _SOAP_tns__AppStart
struct tns__AppStart
{
  void *rubbish; /* ARPDBG */
};
#endif

#ifndef _SOAP_tns__SetTerminationTime
#define _SOAP_tns__SetTerminationTime
struct tns__SetTerminationTime
{
	char *input;
};
#endif

#ifndef _SOAP_tns__FindServiceData
#define _SOAP_tns__FindServiceData
struct tns__FindServiceData
{
	char *input;
};
#endif

#ifndef _SOAP_tns__Attach
#define _SOAP_tns__Attach
struct tns__Attach
{
  void *rubbish; /* ARPDBG */
};
#endif

#ifndef _SOAP_tns__GetControl
#define _SOAP_tns__GetControl
struct tns__GetControl
{
  void *rubbish; /* ARPDBG */
};
#endif

#ifndef _SOAP_SOAP_ENV__Header
#define _SOAP_SOAP_ENV__Header
/* SOAP Header: */
struct SOAP_ENV__Header
{
	void *dummy;	/* transient */
};
#endif

#ifndef _SOAP_SOAP_ENV__Code
#define _SOAP_SOAP_ENV__Code
/* SOAP Fault Code: */
struct SOAP_ENV__Code
{
	char *SOAP_ENV__Value;
	char *SOAP_ENV__Node;
	char *SOAP_ENV__Role;
};
#endif

#ifndef _SOAP_SOAP_ENV__Fault
#define _SOAP_SOAP_ENV__Fault
/* SOAP Fault: */
struct SOAP_ENV__Fault
{
	char *faultcode;
	char *faultstring;
	char *faultactor;
	char *detail;
	struct SOAP_ENV__Code *SOAP_ENV__Code;
	char *SOAP_ENV__Reason;
	char *SOAP_ENV__Detail;
};
#endif

/* Typedefs */
typedef char *xsd__string;
typedef char *xsdl__DestroyResponse;
typedef char *xsdl__GetControlResponse;
typedef char *xsdl__PutControlRequest;
typedef char *xsdl__FindServiceDataResponse;
typedef char *xsdl__DestroyRequest;
typedef char *xsdl__SetTerminationTimeRequest;
typedef char *xsdl__GetNotificationsResponse;
typedef char *xsdl__GetStatusResponse;
typedef char *xsdl__GetNotificationsRequest;
typedef char *xsdl__PauseRequest;
typedef char *xsdl__AppStopRequest;
typedef char *xsdl__AppStopResponse;
typedef char *xsdl__AppStartResponse;
typedef char *xsdl__ResumeRequest;
typedef char *xsdl__DetachResponse;
typedef char *xsdl__StopResponse;
typedef char *xsdl__SetServiceDataResponse;
typedef char *xsdl__FindServiceDataRequest;
typedef char *xsdl__GetStatusRequest;
typedef char *xsdl__SetServiceDataRequest;
typedef char *xsdl__PauseResponse;
typedef char *xsdl__SetTerminationTimeResponse;
typedef char *xsdl__PutControlResponse;
typedef char *xsdl__ResumeResponse;
typedef char *xsdl__GetControlRequest;
typedef char *xsdl__AttachResponse;
typedef char *xsdl__PutStatusResponse;
typedef char *xsdl__AppDetachResponse;
typedef char *xsdl__AppStartRequest;
typedef char *xsdl__AttachRequest;
typedef char *xsdl__AppDetachRequest;
typedef char *xsdl__DetachRequest;
typedef char *xsdl__StopRequest;
typedef char *xsdl__PutStatusRequest;

/* Variables */

/* Remote Methods */

SOAP_FMAC1 int SOAP_FMAC2 tns__GetNotifications(struct soap*, struct tns__GetNotificationsResponse *);

SOAP_FMAC1 int SOAP_FMAC2 tns__AppDetach(struct soap*, struct tns__AppDetachResponse *);

SOAP_FMAC1 int SOAP_FMAC2 tns__Detach(struct soap*, struct tns__DetachResponse *);

SOAP_FMAC1 int SOAP_FMAC2 tns__AppStop(struct soap*, struct tns__AppStopResponse *);

SOAP_FMAC1 int SOAP_FMAC2 tns__SetServiceData(struct soap*, char *, char *, struct tns__SetServiceDataResponse *);

SOAP_FMAC1 int SOAP_FMAC2 tns__Pause(struct soap*, struct tns__PauseResponse *);

SOAP_FMAC1 int SOAP_FMAC2 tns__Stop(struct soap*, struct tns__StopResponse *);

SOAP_FMAC1 int SOAP_FMAC2 tns__PutStatus(struct soap*, char *, struct tns__PutStatusResponse *);

SOAP_FMAC1 int SOAP_FMAC2 tns__GetStatus(struct soap*, struct tns__GetStatusResponse *);

SOAP_FMAC1 int SOAP_FMAC2 tns__Resume(struct soap*, struct tns__ResumeResponse *);

SOAP_FMAC1 int SOAP_FMAC2 tns__PutControl(struct soap*, char *, struct tns__PutControlResponse *);

SOAP_FMAC1 int SOAP_FMAC2 tns__Destroy(struct soap*, struct tns__DestroyResponse *);

SOAP_FMAC1 int SOAP_FMAC2 tns__AppStart(struct soap*, struct tns__AppStartResponse *);

SOAP_FMAC1 int SOAP_FMAC2 tns__SetTerminationTime(struct soap*, char *, struct tns__SetTerminationTimeResponse *);

SOAP_FMAC1 int SOAP_FMAC2 tns__FindServiceData(struct soap*, char *, struct tns__FindServiceDataResponse *);

SOAP_FMAC1 int SOAP_FMAC2 tns__Attach(struct soap*, struct tns__AttachResponse *);

SOAP_FMAC1 int SOAP_FMAC2 tns__GetControl(struct soap*, struct tns__GetControlResponse *);

/* Stubs */

SOAP_FMAC1 int SOAP_FMAC2 soap_call_tns__GetNotifications(struct soap*, const char*, const char*, struct tns__GetNotificationsResponse *);

SOAP_FMAC1 int SOAP_FMAC2 soap_call_tns__AppDetach(struct soap*, const char*, const char*, struct tns__AppDetachResponse *);

SOAP_FMAC1 int SOAP_FMAC2 soap_call_tns__Detach(struct soap*, const char*, const char*, struct tns__DetachResponse *);

SOAP_FMAC1 int SOAP_FMAC2 soap_call_tns__AppStop(struct soap*, const char*, const char*, struct tns__AppStopResponse *);

SOAP_FMAC1 int SOAP_FMAC2 soap_call_tns__SetServiceData(struct soap*, const char*, const char*, char *, char *, struct tns__SetServiceDataResponse *);

SOAP_FMAC1 int SOAP_FMAC2 soap_call_tns__Pause(struct soap*, const char*, const char*, struct tns__PauseResponse *);

SOAP_FMAC1 int SOAP_FMAC2 soap_call_tns__Stop(struct soap*, const char*, const char*, struct tns__StopResponse *);

SOAP_FMAC1 int SOAP_FMAC2 soap_call_tns__PutStatus(struct soap*, const char*, const char*, char *, struct tns__PutStatusResponse *);

SOAP_FMAC1 int SOAP_FMAC2 soap_call_tns__GetStatus(struct soap*, const char*, const char*, struct tns__GetStatusResponse *);

SOAP_FMAC1 int SOAP_FMAC2 soap_call_tns__Resume(struct soap*, const char*, const char*, struct tns__ResumeResponse *);

SOAP_FMAC1 int SOAP_FMAC2 soap_call_tns__PutControl(struct soap*, const char*, const char*, char *, struct tns__PutControlResponse *);

SOAP_FMAC1 int SOAP_FMAC2 soap_call_tns__Destroy(struct soap*, const char*, const char*, struct tns__DestroyResponse *);

SOAP_FMAC1 int SOAP_FMAC2 soap_call_tns__AppStart(struct soap*, const char*, const char*, struct tns__AppStartResponse *);

SOAP_FMAC1 int SOAP_FMAC2 soap_call_tns__SetTerminationTime(struct soap*, const char*, const char*, char *, struct tns__SetTerminationTimeResponse *);

SOAP_FMAC1 int SOAP_FMAC2 soap_call_tns__FindServiceData(struct soap*, const char*, const char*, char *, struct tns__FindServiceDataResponse *);

SOAP_FMAC1 int SOAP_FMAC2 soap_call_tns__Attach(struct soap*, const char*, const char*, struct tns__AttachResponse *);

SOAP_FMAC1 int SOAP_FMAC2 soap_call_tns__GetControl(struct soap*, const char*, const char*, struct tns__GetControlResponse *);

/* Skeletons */

SOAP_FMAC1 int SOAP_FMAC2 soap_serve(struct soap*);

SOAP_FMAC1 int SOAP_FMAC2 soap_serve_tns__GetNotifications(struct soap*);

SOAP_FMAC1 int SOAP_FMAC2 soap_serve_tns__AppDetach(struct soap*);

SOAP_FMAC1 int SOAP_FMAC2 soap_serve_tns__Detach(struct soap*);

SOAP_FMAC1 int SOAP_FMAC2 soap_serve_tns__AppStop(struct soap*);

SOAP_FMAC1 int SOAP_FMAC2 soap_serve_tns__SetServiceData(struct soap*);

SOAP_FMAC1 int SOAP_FMAC2 soap_serve_tns__Pause(struct soap*);

SOAP_FMAC1 int SOAP_FMAC2 soap_serve_tns__Stop(struct soap*);

SOAP_FMAC1 int SOAP_FMAC2 soap_serve_tns__PutStatus(struct soap*);

SOAP_FMAC1 int SOAP_FMAC2 soap_serve_tns__GetStatus(struct soap*);

SOAP_FMAC1 int SOAP_FMAC2 soap_serve_tns__Resume(struct soap*);

SOAP_FMAC1 int SOAP_FMAC2 soap_serve_tns__PutControl(struct soap*);

SOAP_FMAC1 int SOAP_FMAC2 soap_serve_tns__Destroy(struct soap*);

SOAP_FMAC1 int SOAP_FMAC2 soap_serve_tns__AppStart(struct soap*);

SOAP_FMAC1 int SOAP_FMAC2 soap_serve_tns__SetTerminationTime(struct soap*);

SOAP_FMAC1 int SOAP_FMAC2 soap_serve_tns__FindServiceData(struct soap*);

SOAP_FMAC1 int SOAP_FMAC2 soap_serve_tns__Attach(struct soap*);

SOAP_FMAC1 int SOAP_FMAC2 soap_serve_tns__GetControl(struct soap*);
#ifdef __cplusplus
}
#endif
#endif

/* end of soapStub.h */
