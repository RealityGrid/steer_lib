#include <string.h>

void FUNCTION(sizeof_f) ARGS(`STRING_ARG(string), size')
STRING_ARG_DECL(string);
int *size;
{
  char *ptr;

  ptr = STRING_PTR(string);

  if(strcmp(ptr, "short") == 0){

    *size = sizeof(short);
  }
  else if(strcmp(ptr, "int") == 0){

    *size = sizeof(int);
  }
  else if(strcmp(ptr, "long") == 0){
  
    *size = sizeof(long);
  }
  else if(strcmp(ptr, "float") == 0){
  
    *size = sizeof(float);
  }
  else if(strcmp(ptr, "double") == 0){
  
    *size = sizeof(double);
  }
  else{

    printf("sizeof_f: unrecognised c type: %s\n", STRING_PTR(string));
    *size = 0;
  }

  return;
}
