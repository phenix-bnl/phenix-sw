#ifndef COMPRESS_H
#define COMPRESS_H

#include <zlib.h>

int PISAcompress(Bytef *dest, uLongf* destLen, const Bytef* source, uLong sourceLen);

#endif


