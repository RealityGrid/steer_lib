int Get_data_source_address_file(int dummy,
				 char *hostname,
				 int  *port)
{
  char *pchar;
  int   len;

  /* Return port = 0 on failure */
  *port = 0;

  /* Get hostname and port from environment variables */

  pchar = getenv("REG_CONNECTOR_HOSTNAME");
  if (pchar) {
    len = strlen(pchar);
    if (len < REG_MAX_STRING_LENGTH) {
      sprintf(hostname, pchar);
    }
    else{
      fprintf(stderr, "Get_data_source_address_file: content of "
	      "REG_CONNECTOR_HOSTNAME exceeds max. string length of "
	      "%d chars\n", REG_MAX_STRING_LENGTH);
      return REG_FAILURE;
    }
  }
  else{
    fprintf(stderr, 
	    "Get_data_source_address_file: REG_CONNECTOR_HOSTNAME not set\n");
    return REG_FAILURE;
  }

  pchar = getenv("REG_CONNECTOR_PORT");
  if (pchar) {
    *port = atoi(pchar);
  }
  else{
    fprintf(stderr, 
	    "Get_data_source_address_file: REG_CONNECTOR_PORT not set\n");
    return REG_FAILURE;
  }

  return REG_SUCCESS;
}
