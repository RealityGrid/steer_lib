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

/** @file Base64.h
    @brief Header file for Base64-codec routines
  */

#ifndef Base64_H
#define Base64_H

/**
  Encodes len bytes of data pointed to by data as Base64. User 
  must free returned buffer when done.
  @return Pointer to encoded data
  @return Length of encoded buffer (bytes)
 */
int Base64_encode(const char *data, const unsigned int len, 
		  char **out_data, unsigned int *out_len);

/**
  Decodes len bytes of data pointed to by data. User must free
  returned buffer when done.
  @return Pointer to decoded data
  @return Length of decoded buffer (bytes)
 */
int Base64_decode(const char *data, const unsigned int len, 
		  char **out_data, unsigned int *out_len);

#endif
