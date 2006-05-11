/*----------------------------------------------------------------------------
  (C) Copyright 2005, University of Manchester, United Kingdom,
  all rights reserved.

  This software was developed by the RealityGrid project
  (http://www.realitygrid.org), funded by the EPSRC under grants
  GR/R67699/01 and GR/R67699/02.

  LICENCE TERMS

  Redistribution and use in source and binary forms, with or without
  modification, are permitted provided that the following conditions
  are met:
  1. Redistributions of source code must retain the above copyright
     notice, this list of conditions and the following disclaimer.
  2. Redistributions in binary form must reproduce the above copyright
     notice, this list of conditions and the following disclaimer in the
     documentation and/or other materials provided with the distribution.

  THIS MATERIAL IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS
  "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT
  LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR
  A PARTICULAR PURPOSE ARE DISCLAIMED. THE ENTIRE RISK AS TO THE QUALITY
  AND PERFORMANCE OF THE PROGRAM IS WITH YOU.  SHOULD THE PROGRAM PROVE
  DEFECTIVE, YOU ASSUME THE COST OF ALL NECESSARY SERVICING, REPAIR OR
  CORRECTION.
---------------------------------------------------------------------------*/

/** @file ReG_Steer_Utils.c
    @brief Source file for utilities

    This file contains utility routines and data structures for the utilities
    library associated with the ReG computational steering framework.

    @author Andrew Porter
  */

#include "ReG_Steer_types.h"
#include "ReG_Steer_Utils.h"
#include "ReG_Steer_Utils_WSRF.h"
#include "libxml/xmlmemory.h"
#include "libxml/parser.h"

#ifndef WIN32
#include <time.h>
#include <sys/time.h>
#endif /* not WIN32 */

#include "soapH.h"

/*----------------------------------------------------------------*/

char* Create_steering_service(const struct reg_job_details *job,
			      const char                   *containerAddress,
			      const char                   *registryAddress,
			      const struct reg_security_info *sec)
{
#ifdef REG_WSRF
  return Create_SWS(job,
		    containerAddress,
		    registryAddress,
		    sec);
#else
  fprintf(stderr, "Create_steering_service: NOT IMPLEMENTED for OGSI\n");
  return NULL;
#endif /* REG_WSRF */
}

/*----------------------------------------------------------------*/

char *Create_checkpoint_tree(const char *factory, 
			     const char *metadata)
{
  struct rgtf__createNewTreeResponse out;
  char                              *pchar;
  char                              *pend;
  static char                        epr[256];
  struct soap                        soap;

  soap_init(&soap);
  /* Something to do with the XML type */
  soap.encodingStyle = NULL;

  if(soap_call_rgtf__createNewTree(&soap, 
				   factory, 
				   "", /* soap Action */
				   "<ogsi:terminationTime />", "", "", 
				   (char *)metadata, 
				   &out) != SOAP_OK){
    fprintf(stderr, "Create_checkpoint_tree: soap call failed:\n");
    soap_print_fault(&soap, stderr);
    soap_end(&soap); /* dealloc deserialized data */
    soap_done(&soap); /* cleanup and detach soap struct */
    return NULL;
  }

  if( !(pchar = strstr(out._createNewTreeReturn, "<ogsi:handle>")) ){
    fprintf(stderr, "Create_checkpoint_tree: failed to find "
	    "<ogsi:handle> in >>%s<< returned by createNewTree on %s\n", 
	    out._createNewTreeReturn, factory);
    soap_end(&soap); /* dealloc deserialized data */
    soap_done(&soap); /* cleanup and detach soap struct */
    return NULL;
  }

  pchar += 13; /* 13 = strlen("<ogsi:handle>") */
  pend = strchr(pchar, '<');
  strncpy(epr, pchar, (int)(pend-pchar));

  soap_end(&soap); /* dealloc deserialized data */
  soap_done(&soap); /* cleanup and detach soap struct */

  return epr;
}

/*----------------------------------------------------------------*/

int Destroy_steering_service(char                           *address,
                             const struct reg_security_info *sec){
#ifdef REG_WSRF
  return Destroy_WSRP(address, sec);
#else
  fprintf(stderr, "Destroy_steering_service: not implemented for OGSI :-(\n");
  return REG_FAILURE;
#endif

}

/*----------------------------------------------------------------*/

int Get_security_config(const char               *configFile,
			struct reg_security_info *sec){
  char      *pChar;
  char      *pFile = configFile;
  int        len;
  xmlDocPtr  doc;
  xmlNodePtr cur;
  xmlChar   *attrValue;
  FILE      *fp;
  char       bufline[512];

  /* Default to using ~/.realitygrid/security.conf unless we're told
     otherwise */
  if(!configFile || (strlen(configFile) == 0)){
    pChar = getenv("HOME");
    if(!pChar){
      fprintf(stderr, "Get_security_config: cannot get HOME environment "
	      "variable and no alternative config. file specified\n");
      return REG_FAILURE;
    }
    snprintf(bufline, 512, "%s/.realitygrid/security.conf", pChar);
    pFile = bufline;
  }
  else if(strlen(configFile) > 2) { /* minimum length will be ~/a ie 3 */
    int replace = 0;
    if(strncmp(configFile, "$HOME", 5) == 0)
      replace = 5;
    else if(strncmp(configFile, "~", 1) == 0)
      replace = 1;

    if(replace != 0) {
      pChar = getenv("HOME");
      if(!pChar) {
	fprintf(stderr, "Get_security_config: cannot get HOME environment "
		"variable for ~ or $HOME substitution\n");
	return REG_FAILURE;
      }
      snprintf(bufline, 512, "%s%s", pChar, &configFile[replace]);
      pFile = bufline;
    }
    
  }

  /* Set the username to the value of the USER environment variable
     in case we fail to get/parse the certificate for the DN */
  if( (pChar = getenv("USER")) ){
    snprintf(sec->userDN, REG_MAX_STRING_LENGTH, "%s", pChar);
  }

  /* Parse the security.conf file */

  doc = xmlParseFile(pFile);
  if( !(cur = xmlDocGetRootElement(doc)) ){
    printf("Error parsing xml from security.conf: empty document\n");
    xmlFreeDoc(doc);
    xmlCleanupParser();
    return 1;
  }
  if (xmlStrcmp(cur->name, (const xmlChar *) "Security_config")){
    printf("Error parsing xml from security.conf: root element "
           "is not 'Security_config'\n");
    xmlFreeDoc(doc);
    xmlCleanupParser();
    return 1;
  }
  cur = cur->xmlChildrenNode;
  /* Walk the tree - search for first non-blank node */
  while ( cur ){
    if(xmlIsBlankNode ( cur ) ){
      cur = cur -> next;
      continue;
    }
    if( !xmlStrcmp(cur->name, (const xmlChar *)"caCertsPath") ){
      attrValue = xmlGetProp(cur, "value");
      if(attrValue){
        len = xmlStrlen(attrValue);
        strncpy(sec->caCertsPath, (char *)attrValue, len);
        sec->caCertsPath[len] = '\0';
        printf("caCertsPath >>%s<<\n", sec->caCertsPath);
        xmlFree(attrValue);
      }
    }
    else if( !xmlStrcmp(cur->name, (const xmlChar *)"privateKeyCertFile") ){
      attrValue = xmlGetProp(cur, "value");
      if(attrValue){
        len = xmlStrlen(attrValue);
        strncpy(sec->myKeyCertFile, (char *)attrValue, len);
        sec->myKeyCertFile[len] = '\0';
        printf("myKeyCertFile >>%s<<\n", sec->myKeyCertFile);
        xmlFree(attrValue);
      }
    }
    cur = cur->next;
  }

  /* Extract user's DN from their certificate */
  if( !(fp = fopen(sec->myKeyCertFile, "r")) ){

    fprintf(stderr, "Failed to open key and cert file >>%s<<\n",
            sec->myKeyCertFile);
    xmlFreeDoc(doc);
    xmlCleanupParser();
    return REG_FAILURE;
  }

  sec->userDN[0] = '\0';
  while( fgets(bufline, 512, fp) ){
    if(strstr(bufline, "subject=")){
      /* Remove trailing new-line character */
      bufline[strlen(bufline)-1] = '\0';
      pChar = strchr(bufline, '=');
      snprintf(sec->userDN, REG_MAX_STRING_LENGTH, "%s", pChar+1);
      break;
    }
  }
  fclose(fp);
  /*printf("ARPDBG User's DN >>%s<<\n\n", sec->userDN);*/

  xmlFreeDoc(doc);
  xmlCleanupParser();

  return REG_SUCCESS;
}

/*----------------------------------------------------------------*/

int Get_IOTypes(const char                     *address,
		const struct reg_security_info *sec,
		struct reg_iotype_list         *list){
#ifdef REG_WSRF
  return Get_IOTypes_WSRF(address, 
			  sec,
			  list);
#else
  fprintf(stderr, "Destroy_steering_service: not implemented for OGSI :-(\n");
  return REG_FAILURE;
#endif

}
