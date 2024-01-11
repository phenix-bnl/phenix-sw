#include <sys/stat.h>
#include "zutility.h"

time_t get_mtime(const char *fname)
{
  struct stat buf;
  int status = stat(fname, &buf);

  if (status == -1) return 0;

  return buf.st_mtime;
}

