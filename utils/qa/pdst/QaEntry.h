#ifndef __QAENTRY_H__
#define __QAENTRY_H__

// Provides a single place to declare the QaEntry type.
//              Michael P. McCumber
//              mccumber@grad.physics.sunysb.edu
//              8/26/2004

struct QaEntry
{
  double value;
  double error;
  std::string name;
};

#endif /*__QAENTRY_H__ */
