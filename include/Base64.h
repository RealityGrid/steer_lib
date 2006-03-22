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

/** @internal
    @file Base64.h
    @brief Header file for Base64-codec routines
    @author Bob Withers
    @author Andrew Porter
  */

#ifndef Base64_H
#define Base64_H

/** @internal
    @param data Pointer to the data to encode
    @param len Length of data to encode (bytes)
    @param out_data Pointer to malloc'd buffer containing encoded data
    @param out_len Length of output buffer (bytes)

    Base64 encodes @p len bytes of data pointed to by @p data as 
    Base64. User @e must free returned buffer when done.
 */
int Base64_encode(const char *data, const unsigned int len, 
		  char **out_data, unsigned int *out_len);

/** @internal
    @param data Pointer to data to decode
    @param len Amount of data to decode (bytes)
    @param out_data Pointer to buffer containing decoded data
    @param out_len Amount of decoded data (bytes)

    Decodes len bytes of data pointed to by data. User must free
    returned buffer when done.
 */
int Base64_decode(const char *data, const unsigned int len, 
		  char **out_data, unsigned int *out_len);

#endif
