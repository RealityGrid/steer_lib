/*----------------------------------------------------------------------------
  (C) Copyright 2006, University of Manchester, United Kingdom,
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

#include "ReG_Steer_Config.h"
#include "ReG_Steer_types.h"
#include "ReG_Steer_Utils.h"
#include "ReG_Steer_Utils_WSRF.h"
#include "ReG_Steer_Steerside_WSRF.h"
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
  fprintf(stderr, "STEERUtils: Create_steering_service: NOT IMPLEMENTED for OGSI\n");
  return NULL;
#endif /* REG_WSRF */
}

/*----------------------------------------------------------------*/

char *Create_checkpoint_tree(const char *factory, 
			     const char *metadata)
{
#ifdef REG_WSRF

  return Create_checkpoint_tree_wsrf(factory, metadata);

#else

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
    fprintf(stderr, "STEERUtils: Create_checkpoint_tree: soap call failed:\n");
    soap_print_fault(&soap, stderr);
    soap_end(&soap); /* dealloc deserialized data */
    soap_done(&soap); /* cleanup and detach soap struct */
    return NULL;
  }

  if( !(pchar = strstr(out._createNewTreeReturn, "<ogsi:handle>")) ){
    fprintf(stderr, "STEERUtils: Create_checkpoint_tree: failed to find "
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

#endif /* !defined REG_WSRF */
}

/*----------------------------------------------------------------*/

int Destroy_steering_service(const char                     *address,
                             const struct reg_security_info *sec){
#ifdef REG_WSRF
  return Destroy_WSRP(address, sec);
#else
  fprintf(stderr, "STEERUtils: Destroy_steering_service: not implemented for OGSI :-(\n");
  return REG_FAILURE;
#endif

}

/*----------------------------------------------------------------*/

int Get_security_config(const char               *configFile,
			struct reg_security_info *sec){
  char      *pChar;
  int        len;
  xmlDocPtr  doc;
  xmlNodePtr cur;
  xmlChar   *attrValue;
  xmlChar   *attrValueProp;
  FILE      *fp;
  char       bufline[512];

  sec->myKeyCertFile[0] = '\0';
  sec->caCertsPath[0] = '\0';

  /* Default to using ~/.realitygrid/security.conf unless we're told
     otherwise */
  if(!configFile || (strlen(configFile) == 0)){
    pChar = getenv("HOME");
    if(!pChar){
      fprintf(stderr, "STEERUtils: Get_security_config: cannot get HOME environment "
	      "variable and no alternative config. file specified\n");
      return REG_FAILURE;
    }
    snprintf(bufline, 512, "%s/.realitygrid/security.conf", pChar);
  }
  else if(strlen(configFile) > 2) { /* minimum length will be ~/a ie 3 */
    int replace = 0;
    if(strncmp(configFile, "$HOME", 5) == 0)
      replace = 5;
    else if(strncmp(configFile, "${HOME}", 7) == 0)
      replace = 5;
    else if(strncmp(configFile, "~", 1) == 0)
      replace = 1;

    if(replace != 0) {
      pChar = getenv("HOME");
      if(!pChar) {
	fprintf(stderr, "STEERUtils: Get_security_config: cannot get HOME environment "
		"variable for ~ or $HOME substitution\n");
	return REG_FAILURE;
      }
      snprintf(bufline, 512, "%s%s", pChar, &configFile[replace]);
    }    
    else{
      strncpy(bufline, configFile, 512);
    }
  }

  /* Set the username to the value of the USER environment variable
     in case we fail to get/parse the certificate for the DN */
  if( (pChar = getenv("USER")) ){
    snprintf(sec->userDN, REG_MAX_STRING_LENGTH, "%s", pChar);
  }

  /* Parse the security.conf file */

  doc = xmlParseFile(bufline);
  if( !(cur = xmlDocGetRootElement(doc)) ){
    printf("Error parsing xml from security.conf: empty document\n");
    xmlFreeDoc(doc);
    xmlCleanupParser();
    return REG_FAILURE;
  }
  if (xmlStrcmp(cur->name, (const xmlChar *) "Security_config")){
    printf("Error parsing xml from security.conf: root element "
           "is not 'Security_config'\n");
    xmlFreeDoc(doc);
    xmlCleanupParser();
    return REG_FAILURE;
  }
  cur = cur->xmlChildrenNode;
  /* Walk the tree - search for first non-blank node */
  while ( cur ){
    if(xmlIsBlankNode ( cur ) ){
      cur = cur -> next;
      continue;
    }
    if( !xmlStrcmp(cur->name, (const xmlChar *)"caCertsPath") ){
      attrValue = xmlGetProp(cur, (const xmlChar*) "value");
      if(attrValue){
        len = xmlStrlen(attrValue);
        strncpy(sec->caCertsPath, (char *)attrValue, len);
        sec->caCertsPath[len] = '\0';
#ifdef REG_DEBUG
        printf("Get_security_config: caCertsPath >>%s<<\n", sec->caCertsPath);
#endif
        xmlFree(attrValue);
      }
    }
    else if( !xmlStrcmp(cur->name, (const xmlChar *)"privateKeyCertFile") ){
      attrValue = xmlGetProp(cur, (const xmlChar*) "value");
      if(attrValue){
        len = xmlStrlen(attrValue);
        strncpy(sec->myKeyCertFile, (char *)attrValue, len);
        sec->myKeyCertFile[len] = '\0';
#ifdef REG_DEBUG
        printf("Get_security_config: myKeyCertFile >>%s<<\n", 
	       sec->myKeyCertFile);
#endif
        xmlFree(attrValue);
      }
    }
    cur = cur->next;
  }

  /* Config file exists but does not specify any path to CA certs
     or key+cert file */
  if(!(sec->myKeyCertFile[0]) || !(sec->caCertsPath[0])){
    xmlFreeDoc(doc);
    xmlCleanupParser();
    return REG_FAILURE;    
  }

  /* Extract user's DN from their certificate */
  if( !(fp = fopen(sec->myKeyCertFile, "r")) ){

    fprintf(stderr, "STEERUtils: Failed to open key and cert file >>%s<<\n",
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
#ifdef REG_DEBUG
  printf("Get_security_config: User's DN >>%s<<\n\n", sec->userDN);
#endif

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
  fprintf(stderr, "STEERUtils: Get_IOTypes: not implemented for "
	  "OGSI :-(\n");
  return REG_FAILURE;
#endif

}

/*----------------------------------------------------------------*/

int Delete_iotype_list(struct reg_iotype_list *list)
{
  if(!list)return REG_FAILURE;

  if(list->iotype)free(list->iotype);
  list->iotype = NULL;
  list->numEntries = 0;

  return REG_SUCCESS;
}

/*----------------------------------------------------------------*/

int Set_service_data_source(const char *EPR, 
			    const char *sourceAddress, 
			    const int   sourcePort, 
			    const char *label,
			    const struct reg_security_info *sec)
{
  char        buf[1024];
  struct soap mySoap;
  int         status;

  if(sourcePort == 0){
    /* No proxy being used */
    snprintf(buf, 1024, "<dataSource><sourceEPR>%s</sourceEPR>"
	     "<sourceLabel>%s</sourceLabel></dataSource>",
	     sourceAddress, label);
  }
  else{
    /* A proxy has been specified */
    snprintf(buf, 1024, "<dataSource><Proxy><address>%s"
	     "</address><port>%d</port></Proxy><sourceLabel>%s"
	     "</sourceLabel></dataSource>",
	     sourceAddress, sourcePort, label);
  }

  soap_init(&mySoap);

  status = Set_resource_property(&mySoap, EPR, sec->userDN,
				 sec->passphrase, buf);

  soap_end(&mySoap);
  soap_done(&mySoap);

  return status;
}
