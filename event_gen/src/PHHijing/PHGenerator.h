
#ifndef __PHGENERATOR_H__
#define __PHGENERATOR_H__

#include <TGenerator.h>

// This class was written because TGenerator doesn't have things like
// a separate initialization, nor a more generic SetParameter that
// takes a character string value arg.  This second example was
// important because the HIJING configuration requires some inputs be
// strings, like the projectile and target.

class PHGenerator : public TGenerator
{
public:
  PHGenerator(const char* name, const char* title) : TGenerator(name,title) {}
  virtual ~PHGenerator() {}

  virtual void SetParameter(const char* name, double val) {};
  virtual void SetParameter(const char* name, const char* chval) = 0;

  virtual void SetDecay(int pid, bool flag) = 0;

  virtual void Init() = 0;

private:
};

#endif
