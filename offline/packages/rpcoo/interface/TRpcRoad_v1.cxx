/*!
	\file TRpcRoad_v1.cxx
	\brief The RPC Track object 
	\author R. S. Hollis
  \version $Revision: 1.2 $
  \date    $Date: 2010/11/29 20:21:10 $
*/

#include "TRpcRoad_v1.h"
#include "RPCOO.h"

#include <iomanip>

//_______________________________________
TRpcRoad_v1::TRpcRoad_v1()
{
  fArm = 0;
  fIndex = 0;
  fMuiRoad = NULL;
  fGolden = true;
  fDCA1 = 0;
  fDCA3 = 0;
  fRpcClus1 = NULL;
  fRpcClus3 = NULL;
  fKeepMe3 = true;
}

//__________________________________________________  
TRpcRoad_v1::TRpcRoad_v1(const Key& key, UShort_t arm, UShort_t index) : TRpcRoad(key)
{
  fArm = arm;
  fIndex = index;
  fMuiRoad = NULL;
  fGolden = true;
  fDCA1 = 0;
  fDCA3 = 0;
  fRpcClus1 = NULL;
  fRpcClus3 = NULL;
  fKeepMe3 = true;
}

//__________________________________________________  
TRpcRoad_v1::TRpcRoad_v1(const TRpcRoad* base_ptr) : TRpcRoad(*base_ptr)
{
  fArm = base_ptr->get_arm();
  fIndex = base_ptr->get_index(); 
  fMuiRoad = base_ptr->get_muiroad();
  fGolden = base_ptr->get_golden();
  fDCA1 = base_ptr->get_dca1();
  fDCA3 = base_ptr->get_dca3();
  fRpcClus1 = base_ptr->get_rpcclus1();
  fRpcClus3 = base_ptr->get_rpcclus3();
  fKeepMe3 = base_ptr->get_lowdca3();
}

//__________________________________________________  
TRpcRoad_v1::TRpcRoad_v1(const TRpcRoad& base_ref) : TRpcRoad(base_ref)
{
  fArm = base_ref.get_arm();
  fIndex = base_ref.get_index();
  fMuiRoad = base_ref.get_muiroad();
  fGolden = base_ref.get_golden();
  fDCA1 = base_ref.get_dca1();
  fDCA3 = base_ref.get_dca3();
  fRpcClus1 = base_ref.get_rpcclus1();
  fRpcClus3 = base_ref.get_rpcclus3();
  fKeepMe3 = base_ref.get_lowdca3();
}

