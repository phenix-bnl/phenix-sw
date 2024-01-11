#ifndef __EMCFEMTUPLEFACTORY_H__
#define __EMCFEMTUPLEFACTORY_H__

class emcFEMtuple;

/** (OLD,Factory) Build emcFEMtuple objects. 
 *
 * \deprecated
 * @ingroup oldemccalib
 */

class emcFEMtupleFactory
{
 public:
  /// Create a void emcFEMtuple object of a given category.
  static emcFEMtuple* Create(const char* category);
};

#endif
