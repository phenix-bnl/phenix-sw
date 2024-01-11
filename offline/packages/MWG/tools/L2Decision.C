#include "Tools.h"
#include <MuonUtil.h>
#include <MUTOO.h>
#include <PHCompositeNode.h>

using namespace std;

//_______________________________________________________
boost::array< bool, 2 > Tools::_l2_muid_decision = {{ false }};
boost::array< bool, 2 > Tools::_l2_mutr_decision = {{ false }};

//_______________________________________________________
void Tools::LoadL2Decision( PHCompositeNode *top_node )
{
  // cout << "Tools::LoadL2Decision" << endl;
  _l2_muid_decision[MUTOO::South] = MuonUtil::get_l2_trigger_decision( top_node, "L2MuidDimuonSouthTrigger" );
  _l2_muid_decision[MUTOO::North] = MuonUtil::get_l2_trigger_decision( top_node, "L2MuidDimuonNorthTrigger" );
  
  _l2_mutr_decision[MUTOO::South] = MuonUtil::get_l2_trigger_decision( top_node, "L2MutrDimuonSouthTrigger" );
  _l2_mutr_decision[MUTOO::North] = MuonUtil::get_l2_trigger_decision( top_node, "L2MutrDimuonNorthTrigger" );
}

//_______________________________________________________
bool Tools::L2MuidDecision( const unsigned int& arm )
{ return _l2_muid_decision[arm]; }

//_______________________________________________________
bool Tools::L2MutrDecision( const unsigned int& arm )
{ return _l2_mutr_decision[arm]; }
