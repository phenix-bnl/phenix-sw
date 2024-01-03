
/*!
  \file    PHGlobal_Muonv1.C
  \brief   Muon specific global variables
  \author  Hugo Pereira
  \version $Revision: 1.3 $
  \date    $Date: 2012/09/12 07:23:12 $
*/

#include "PHGlobal_Muonv1.h"

using namespace std;

ClassImp(PHGlobal_Muonv1)

//__________________________________________________
void PHGlobal_Muonv1::identify(std::ostream& os) const
{
  os << "identify yourself: PHGlobal_Muonv1 Object, Global Event Information." << std::endl;
}

//__________________________________________________
int PHGlobal_Muonv1::isValid() const
{
	return
			get_nMutrHits(0,0)>=0 && get_nMutrHits(1,0)>=0 &&
			get_nMuidHits(0,0)>=0 && get_nMuidHits(1,0)>=0;
}

