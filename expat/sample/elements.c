/* This is simple demonstration of how to use expat. This program
reads an XML document from standard input and writes a line with the
name of each element to standard output indenting child elements by
one tab stop more than their parent element. */

#include <stdio.h>
#include "xmlparse.h"

void startElement(void *userData, const char *name, const char **atts)
{
  int i;
  int *depthPtr = userData;
  for (i = 0; i < *depthPtr; i++)
    putchar('\t');
  puts(name);

  *depthPtr += 1;
}

void endElement(void *userData, const char *name)
{
  int *depthPtr = userData;
  *depthPtr -= 1;
}

void dataHandler(void *userData, const XML_Char *s, int len)
{
  int i;
  int *depthPtr = userData;
  
  if(*depthPtr > 2){

    printf("Field value = ");

    for(i=0; i<len; i++){
    
      putchar(s[i]);
    }

    putchar('\n');
  }
}

int main()
{
  char       buf[BUFSIZ];
  XML_Parser parser = XML_ParserCreate(NULL);
  int        done;
  int        depth = 0;
  FILE      *fp;

  XML_SetUserData(parser, &depth);
  XML_SetElementHandler(parser, startElement, endElement);
  XML_SetCharacterDataHandler(parser, dataHandler);

  fp = fopen("status_info_0", "r");

  if(fp == NULL){

    printf ("Could not open file :(\n");
    return 1;
  }

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

  fclose(fp);

  return 0;
}
