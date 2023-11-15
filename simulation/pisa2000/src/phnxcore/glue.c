
#include "compress.h"
#include <zlib.h>
#include <string.h>
#include <stdio.h>

#define ARRAYLENGTH 100000

//_________________________________________
struct output
{
  short id;
  short length;
  Bytef buffer[ARRAYLENGTH];
};

char filename[256];

FILE *fp;

//_________________________________________
int lzoutfile_(char *file, int len)
{
  strncpy( filename, file, len);
  printf ("output file = %s\n", filename);
  if ( (fp = fopen(filename, "w")) ==NULL ) return -1;
  else return 0;
}

//_________________________________________
int lzinfile_(char *file, int len)
{
  strncpy( filename, file, len);
  printf ("input file = %s\n", filename);
  if ( (fp = fopen(filename, "r")) ==NULL ) return -1;
  else return 0;
}

//_________________________________________
void lzclose_(void)
{ fclose(fp); }

static struct output out;

//_________________________________________
// Change uLongf iquest to int iquest in order to work on DEC Alpha
void compwrite_ (Bytef * ibuf, int iquest[], int *nw)
{
  uLongf iout;
  
  iout = ARRAYLENGTH;
  PISAcompress (out.buffer, &iout, ibuf, 4*iquest[1]);
  /* printf("len= %d  %d\n", 4*iquest[1], iout); */
  out.id = 0xAC;
  out.length = iout;
  iout += 4;
  fwrite (&out, iout,1 , fp);
  *nw =  iout;
  
}

//_________________________________________
// Change uLongf iquest to int iquest in order to work on DEC Alpha
void compread_ (Bytef * ibuf, int iquest[], int *nw)
{
  uLongf iout, in;
  fread (&out, 4,1 , fp);
  if (feof(fp))
  {
    iquest[0] = -1;
    return;
  }
  if (out.id != 0xAC)
  {
    out.length = ((out.length & 0xff)<<8 | (out.length & 0xff00)>>8);
  }
  
  fread (out.buffer, out.length, 1,fp);
  if (feof(fp))
  {
    iquest[0] = -1;
    return;
  }
  
  in = out.length;
  iout = 4*iquest[1];
  uncompress (ibuf, &iout, out.buffer, in);
  
  /* printf("len= %d  %d\n", in, iout); */
  iquest[0] = 0;
  iquest[1] = iout;
  *nw =  iout;
  
}
