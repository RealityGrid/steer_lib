/* This is simple demonstration of how to use expat. This program
reads an XML document from standard input and writes a line with the
name of each element to standard output indenting child elements by
one tab stop more than their parent element. */

#include <stdio.h>
#include "xmlparse.h"

#define NUM_ENTRIES 10
#define REG_MAX_STRING_LENGTH 40

typedef struct {

  char  label[REG_MAX_STRING_LENGTH];
  int   steerable;
  int   type;
  int   handle;
  char  value[REG_MAX_STRING_LENGTH];

} param_entry;

typedef struct {

  int          num_registered;
  int          max_entries;
  param_entry *param;

} table_type;

typedef struct {

  enum {NOTSET = 0,
	LABEL,
	STRABLE,
	TYPE,
	HANDLE,
	VALUE
  }field_type;

  char       field[REG_MAX_STRING_LENGTH];
  table_type Params_table;

} user_data_type;

/*-----------------------------------------------------------------*/

void startElement(void *userData, const char *name, const char **atts)
{
  int             i;
  user_data_type *data = userData;

  if(strcmp(name, "Label") == 0){
    data->field_type = LABEL;
  }
  else if(strcmp(name, "Steerable") == 0){
    data->field_type = STRABLE;
  }
  else if(strcmp(name, "Type") == 0){
    data->field_type = TYPE;
  }
  else if(strcmp(name, "Handle") == 0){
    data->field_type = HANDLE;
  }
  else if(strcmp(name, "Value") == 0){
    data->field_type = VALUE;
  }

}

/*-----------------------------------------------------------------*/

void endElement(void *userData, const char *name)
{
  user_data_type *data = userData;

  if(data->field_type != NOTSET) data->field_type = NOTSET;

  if(strcmp(name, "Param") == 0){
    printf("End of Param section\n");
    data->Params_table.num_registered++;
  }
}

/*-----------------------------------------------------------------*/

void dataHandler(void *userData, const XML_Char *s, int len)
{
  int             i;
  char            buf[REG_MAX_STRING_LENGTH];
  user_data_type *data = userData;
  
  /* Create null-terminated string from input array of XML_Char */
  strncpy(buf, s, len);
  buf[len] = 0;

  if(data->field_type != NOTSET){

    printf("Field value = %s\n", buf);

    i = data->Params_table.num_registered;

    if(data->field_type == LABEL){
      strcpy(data->Params_table.param[i].label, buf);
    }
    else if(data->field_type == STRABLE){
      sscanf(buf , "%d", &(data->Params_table.param[i].steerable) );
    }
    else if(data->field_type == TYPE){
      sscanf(buf , "%d", &(data->Params_table.param[i].type) );
    }
    else if(data->field_type == HANDLE){
      sscanf(buf , "%d", &(data->Params_table.param[i].handle) );
    }
    else if(data->field_type == VALUE){

      strcpy(data->Params_table.param[i].value, buf);      
    }
  }
}

int main()
{
  int            i;
  char           buf[BUFSIZ];
  XML_Parser     parser = XML_ParserCreate(NULL);
  int            done;
  FILE          *fp;
  user_data_type User_data;

  XML_SetUserData(parser, &User_data);
  XML_SetElementHandler(parser, startElement, endElement);
  XML_SetCharacterDataHandler(parser, dataHandler);

  User_data.Params_table.param = (param_entry *)malloc(NUM_ENTRIES*
						       sizeof(param_entry));

  if(User_data.Params_table.param == NULL){

    printf("Failed to allocate memory\n");
    return 1;
  }

  fp = fopen("status_info_0", "r");

  if(fp == NULL){

    printf ("Could not open file :(\n");
    return 1;
  }

  User_data.field_type = NOTSET;

  do {
    /* size_t len = fread(buf, 1, sizeof(buf), stdin); */
    size_t len = fread(buf, 1, sizeof(buf), fp);
    done = len < sizeof(buf);

    if (!XML_Parse(parser, buf, len, done)) {
      fprintf(stderr,
	      "%s at line %d\n",
	      XML_ErrorString(XML_GetErrorCode(parser)),
	      XML_GetCurrentLineNumber(parser));
      return 1;
    }
  } while (!done);
  XML_ParserFree(parser);

  for(i=0; i<User_data.Params_table.num_registered; i++){

    printf("%d: %s\n", i, User_data.Params_table.param[i].label);
    printf("    Steerable = %d\n", User_data.Params_table.param[i].steerable);
    printf("    Type      = %d\n", User_data.Params_table.param[i].type);
    printf("    Handle    = %d\n", User_data.Params_table.param[i].handle);
    printf("    Value     = %s\n\n", User_data.Params_table.param[i].value);
  }

  fclose(fp);

  return 0;
}
