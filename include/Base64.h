
/*********************************************************************
* C_Base64 - a simple base64 encoder and decoder.
*
*     Copyright (c) 1999, Bob Withers - bwit@pobox.com
*
* This code may be freely used for any purpose, either personal
* or commercial, provided the authors copyright notice remains
* intact.
*
* Converted from C++ to C by Andrew Porter
*********************************************************************/

#ifndef Base64_H
#define Base64_H

int Base64_encode(const char *data, const unsigned int len, 
		  char **out_data, unsigned int *out_len);

int Base64_decode(const char *data, const unsigned int len, 
		  char **out_data, unsigned int *out_len);

#endif
