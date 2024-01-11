#ifndef __FKINDUMPER_H__
#define __FKINDUMPER_H__

#include "fkinWrapper.h"
#include "primaryWrapper.h"

#include <iostream>

class fkinDumper 
{
 public:
  
  static void dump(const fkinWrapper& fkin, const char* title,
		   std::ostream& os = std::cout);

  static void dump(const primaryWrapper& primary, const char* title,
		   std::ostream& os = std::cout);
};

#endif
