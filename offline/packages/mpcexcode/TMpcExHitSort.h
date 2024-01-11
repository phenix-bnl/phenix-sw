#ifndef __TMPCEXHITSORT_H__
#define __TMPCEXHITSORT_H__

#include <functional>
#include "TMpcExHit.h"

namespace TMpcExHitSort {

  struct SortByKey:
    public std::binary_function<const TMpcExHit *, const TMpcExHit *, bool> {

    bool operator() (const TMpcExHit *lhs, const TMpcExHit *rhs) const
    {
      return  ( lhs->key() < rhs->key() );
    }
  };

  struct SortByArm:
    public std::binary_function<const TMpcExHit *, const TMpcExHit *, bool> {

    bool operator() (const TMpcExHit *lhs, const TMpcExHit *rhs) const
    {
      return  ( lhs->arm() < rhs->arm() );
    }
  };

  struct SortByArmAndLayer:
    public std::binary_function<const TMpcExHit *, const TMpcExHit *, bool> {

    bool operator() (const TMpcExHit *lhs, const TMpcExHit *rhs) const
    {
      return  ( lhs->arm() < rhs->arm() && lhs->layer() < rhs->layer() );
    }
  };

}

#endif /* __TMPCEXHITSORT_H__ */
