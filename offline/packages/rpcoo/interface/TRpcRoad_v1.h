#ifndef __TRpcRoad_v1_H__
#define __TRpcRoad_v1_H__

/*!
	\file TRpcRoad_v1.h
	\brief The RPC Track object 
	\author R. S. Hollis
  \version $Revision: 1.3 $
  \date    $Date: 2010/11/29 20:21:10 $
*/

#include "TRpcRoad.h"
#include <vector>

/*! @ingroup interface */
//!  The RPC Road object 

/*! 
  <b>The RPC Road Object</b><br>

  The RPC Road object presents an interface to mui road parameter
  objects.

  <p>
  To access all the hits associated with a road one uses the standard
  syntax for accessing associated objects in MUTOO.
*/

class TMuiRoadO;
class TRpcClus;

class TRpcRoad_v1 : public TRpcRoad
{
  
 public:

  //! @name Constructors/Destructors
  //@{    

  //! constructor 
  TRpcRoad_v1();

  //! constructor 
  TRpcRoad_v1(const Key&, UShort_t arm, UShort_t index);

  //! constructor 
  TRpcRoad_v1(const TRpcRoad* base_ptr);

  //! constructor 
  TRpcRoad_v1(const TRpcRoad& base_ref);

  //! Destructor 
  virtual ~TRpcRoad_v1()
  {}

  //@}


  TMuiRoadO *fMuiRoad;
  bool fGolden;
  int fArm;
  int fIndex;

  float fDCA1;
  float fDCA3;
  TRpcClus *fRpcClus1;
  TRpcClus *fRpcClus3;

  bool fKeepMe3;

  void set_arm(int arm)             { fArm = arm;       }
  void set_index(int index)         { fIndex = index;   }
  void set_muiroad(TMuiRoadO *road) { fMuiRoad = road;  }
  void set_golden(bool golden)      { fGolden = golden; }
  void set_dca1(float dca)          { fDCA1 = dca;      }
  void set_dca3(float dca)          { fDCA3 = dca;      }
  void set_rpcclus1(TRpcClus *clus) { fRpcClus1 = clus; }
  void set_rpcclus3(TRpcClus *clus) { fRpcClus3 = clus; }
  void set_lowdca3(bool keepme)     { fKeepMe3= keepme; }

  int get_arm()            const { return fArm;      }
  int get_index()          const { return fIndex;    }
  TMuiRoadO* get_muiroad() const { return fMuiRoad;  }
  bool get_golden()        const { return fGolden;   }
  float get_dca1()         const { return fDCA1;     }
  float get_dca3()         const { return fDCA3;     }
  TRpcClus* get_rpcclus1() const { return fRpcClus1; }
  TRpcClus* get_rpcclus3() const { return fRpcClus3; }
  bool get_lowdca3()       const { return fKeepMe3;  }

  ClassDef(TRpcRoad_v1,1)
};
  
#endif 


