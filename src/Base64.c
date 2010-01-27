/*********************************************************************
* Base64 - a simple base64 encoder and decoder.
*
*     Copyright (c) 1999, Bob Withers - bwit@pobox.com
*
* This code may be freely used for any purpose, either personal
* or commercial, provided the authors copyright notice remains
* intact.
*
* Enhancements by Stanley Yamane:
*     o reverse lookup table for the decode function
*     o reserve string buffer space in advance
*
* Converted from C++ to C by Andrew Porter
*
*********************************************************************/

/** @internal
    @file Base64.c
    @brief Source file for Base64-codec routines
    @author Bob Withers
    @author Andrew Porter
*/

#include "ReG_Steer_Config.h"
#include "Base64.h"
#include "ReG_Steer_types.h"

static const char   fillchar = '=';
#define Base64_NP 257

                          /* 0   0   0   0   0   0   0   0   0   0   111111111122222222223333333333444444444455555555556666
                             0   1   2   3   4   5   6   7   8   9   012345678901234567890123456789012345678901234567890123 */
const char Base64Table[] = {'A','B','C','D','E','F','G','H','I','J','K','L','M','N','O','P','Q','R','S','T','U','V','W','X','Y','Z','a','b','c','d','e','f','g','h','i','j','k','l','m','n','o','p','q','r','s','t','u','v','w','x','y','z','0','1','2','3','4','5','6','7','8','9','+','/'};

/**
  Decode Table gives the index of any valid base64 character in the Base64 table]
  65 == A, 97 == a, 48 == 0, 43 == +, 47 == /
*/
                                   /*   0         1         2         3         4         5         6         7         8         9 */
const unsigned int DecodeTable[] = {Base64_NP,Base64_NP,Base64_NP,Base64_NP,Base64_NP,Base64_NP,Base64_NP,Base64_NP,Base64_NP,Base64_NP,  /* 0 - 9  */
                                    Base64_NP,Base64_NP,Base64_NP,Base64_NP,Base64_NP,Base64_NP,Base64_NP,Base64_NP,Base64_NP,Base64_NP,  /* 10 -19 */
                                    Base64_NP,Base64_NP,Base64_NP,Base64_NP,Base64_NP,Base64_NP,Base64_NP,Base64_NP,Base64_NP,Base64_NP,  /* 20 -29 */
                                    Base64_NP,Base64_NP,Base64_NP,Base64_NP,Base64_NP,Base64_NP,Base64_NP,Base64_NP,Base64_NP,Base64_NP,  /* 30 -39 */
                                    Base64_NP,Base64_NP,Base64_NP,62,Base64_NP,Base64_NP,Base64_NP,63,52,53,  /* 40 -49 */
                                    54,55,56,57,58,59,60,61,Base64_NP,Base64_NP,  /* 50 -59 */
                                    Base64_NP,Base64_NP,Base64_NP,Base64_NP,Base64_NP, 0, 1, 2, 3, 4,  /* 60 -69 */
                                     5, 6, 7, 8, 9,10,11,12,13,14,  /* 70 -79 */
                                    15,16,17,18,19,20,21,22,23,24,  /* 80 -89 */
                                    25,Base64_NP,Base64_NP,Base64_NP,Base64_NP,Base64_NP,Base64_NP,26,27,28,  /* 90 -99 */
                                    29,30,31,32,33,34,35,36,37,38,  /* 100 -109 */
                                    39,40,41,42,43,44,45,46,47,48,  /* 110 -119 */
                                    49,50,51,Base64_NP,Base64_NP,Base64_NP,Base64_NP,Base64_NP,Base64_NP,Base64_NP,  /* 120 -129 */
                                    Base64_NP,Base64_NP,Base64_NP,Base64_NP,Base64_NP,Base64_NP,Base64_NP,Base64_NP,Base64_NP,Base64_NP,  /* 130 -139 */
                                    Base64_NP,Base64_NP,Base64_NP,Base64_NP,Base64_NP,Base64_NP,Base64_NP,Base64_NP,Base64_NP,Base64_NP,  /* 140 -149 */
                                    Base64_NP,Base64_NP,Base64_NP,Base64_NP,Base64_NP,Base64_NP,Base64_NP,Base64_NP,Base64_NP,Base64_NP,  /* 150 -159 */
                                    Base64_NP,Base64_NP,Base64_NP,Base64_NP,Base64_NP,Base64_NP,Base64_NP,Base64_NP,Base64_NP,Base64_NP,  /* 160 -169 */
                                    Base64_NP,Base64_NP,Base64_NP,Base64_NP,Base64_NP,Base64_NP,Base64_NP,Base64_NP,Base64_NP,Base64_NP,  /* 170 -179 */
                                    Base64_NP,Base64_NP,Base64_NP,Base64_NP,Base64_NP,Base64_NP,Base64_NP,Base64_NP,Base64_NP,Base64_NP,  /* 180 -189 */
                                    Base64_NP,Base64_NP,Base64_NP,Base64_NP,Base64_NP,Base64_NP,Base64_NP,Base64_NP,Base64_NP,Base64_NP,  /* 190 -199 */
                                    Base64_NP,Base64_NP,Base64_NP,Base64_NP,Base64_NP,Base64_NP,Base64_NP,Base64_NP,Base64_NP,Base64_NP,  /* 200 -209 */
                                    Base64_NP,Base64_NP,Base64_NP,Base64_NP,Base64_NP,Base64_NP,Base64_NP,Base64_NP,Base64_NP,Base64_NP,  /* 210 -219 */
                                    Base64_NP,Base64_NP,Base64_NP,Base64_NP,Base64_NP,Base64_NP,Base64_NP,Base64_NP,Base64_NP,Base64_NP,  /* 220 -229 */
                                    Base64_NP,Base64_NP,Base64_NP,Base64_NP,Base64_NP,Base64_NP,Base64_NP,Base64_NP,Base64_NP,Base64_NP,  /* 230 -239 */
                                    Base64_NP,Base64_NP,Base64_NP,Base64_NP,Base64_NP,Base64_NP,Base64_NP,Base64_NP,Base64_NP,Base64_NP,  /* 240 -249 */
                                    Base64_NP,Base64_NP,Base64_NP,Base64_NP,Base64_NP,Base64_NP               /* 250 -256 */
				        			  };


int Base64_encode(const char *data,
		  const unsigned int len,
		  char **out_data,
		  unsigned int *out_len)
{
  unsigned int  i;
  char          c;
  char         *pchar;

  *out_data = (char *)malloc(len*2.5);
  if(!*out_data){
    fprintf(stderr, "STEER: Base64_encode: malloc failed\n");
    return REG_FAILURE;
  }

  pchar = *out_data;
  for (i = 0; i < len; ++i) {

    c = (data[i] >> 2) & 0x3f;
    *(pchar++) = Base64Table[(int)c];
    c = (data[i] << 4) & 0x3f;
    if (++i < len)
      c |= (data[i] >> 4) & 0x0f;

    *(pchar++) = Base64Table[(int)c];
    if (i < len) {

      c = (data[i] << 2) & 0x3f;
      if (++i < len)
	c |= (data[i] >> 6) & 0x03;

      *(pchar++) = Base64Table[(int)c];
    }
    else{

      ++i;
      *(pchar++) = fillchar;
    }

    if (i < len) {

      c = data[i] & 0x3f;
      *(pchar++) = Base64Table[(int)c];
    }
    else{

      *(pchar++) = fillchar;
    }
  }

  *out_len = (unsigned int)(pchar - *out_data);
  return REG_SUCCESS;
}

int Base64_decode(const char *data,
		  const unsigned int len,
		  char **out_data,
		  unsigned int *out_len)
{
  unsigned int  i;
  char c;
  char c1;
  char *pchar;

  if(!*out_data){
    *out_data = (char *)malloc(len);
    if(!*out_data){
      fprintf(stderr, "STEER: Base64_decode: malloc failed\n");
      return REG_FAILURE;
    }
  }

  pchar = *out_data;

  for (i = 0; i < len; ++i){

    c = (char) DecodeTable[(unsigned char)data[i]];
    ++i;
    c1 = (char) DecodeTable[(unsigned char)data[i]];
    c = (c << 2) | ((c1 >> 4) & 0x3);
    *(pchar++) = c;
    if (++i < len){

      if (fillchar == data[i]) break;

      c = (char) DecodeTable[(unsigned char)data[i]];
      c1 = ((c1 << 4) & 0xf0) | ((c >> 2) & 0xf);
      *(pchar++) =  c1;
    }

    if (++i < len) {

      if (fillchar == data[i]) break;

      c1 = (char) DecodeTable[(unsigned char)data[i]];
      c = ((c << 6) & 0xc0) | c1;
      *(pchar++) = c;
    }
  }
  *out_len = (unsigned int)(pchar - *out_data);
  return REG_SUCCESS;
}
