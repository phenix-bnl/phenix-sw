#ifndef __EMC_GEACLUSTER_CONTENT_V6_H__
#define __EMC_GEACLUSTER_CONTENT_V6_H__





#include <emcClusterContentv6.h>
#include <emcGeaClusterContentT.h>


class emcGeaClusterContentv6: public emcGeaClusterContentT< emcClusterContentv6 > {
public:
  emcGeaClusterContentv6(emc_clusterid_t clusterid = EMC_INVALID_CLUSTERID){ set_id(clusterid); }
  virtual ~emcGeaClusterContentv6(){}
  virtual emcGeaClusterContentv6 * create(void) const { return new emcGeaClusterContentv6; };

  // noop: no cached data at all.
  virtual void invcache(cachetype_t type = CACHED_ALL){ }


public:
  ClassDef(emcGeaClusterContentv6, 1)
};





#endif /* ! __EMC_GEACLUSTER_CONTENT_V6_H__ */

