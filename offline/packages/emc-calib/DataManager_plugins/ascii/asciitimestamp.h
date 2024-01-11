#ifndef __ASCIITIMESTAMP_H__
#define __ASCIITIMESTAMP_H__

#include <fstream>
#ifndef __PHTIMESTAMP_H__
#include "PHTimeStamp.h"
#endif

PHTimeStamp getTimeStamp(const char* thedate);
PHTimeStamp getTimeStamp(std::ifstream& in);

#endif


