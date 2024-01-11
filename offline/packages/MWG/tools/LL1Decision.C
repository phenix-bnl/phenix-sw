#include "Tools.h"
#include <MuonUtil.h>
#include <PHCompositeNode.h>

using namespace std;

//_______________________________________________________
boost::array< bool, 2 > Tools::_1S_ll1_decision = {{ false }};

//_______________________________________________________
boost::array< bool, 2 > Tools::_1D_ll1_decision = {{ false }};

//_______________________________________________________
boost::array< bool, 2 > Tools::_2D_ll1_decision = {{ false }};

//_______________________________________________________
boost::array< bool, 2 > Tools::_1D1S_ll1_decision = {{ false }};

//_______________________________________________________
void Tools::LoadLL1Decision( PHCompositeNode *top_node )
{
  for( unsigned int arm = 0; arm < 2; arm++ )
  {
    _1S_ll1_decision[arm] = MuonUtil::get_1S_LL1trigger_decision( top_node, arm );
    _1D_ll1_decision[arm] = MuonUtil::get_1D_LL1trigger_decision( top_node, arm );
    _2D_ll1_decision[arm] = MuonUtil::get_2D_LL1trigger_decision( top_node, arm );
    _1D1S_ll1_decision[arm] = MuonUtil::get_1D1S_LL1trigger_decision( top_node, arm );
  }
}

//_______________________________________________________
bool Tools::LL1_1S_Decision( const unsigned int& arm )
{ return _1S_ll1_decision[arm]; }

//_______________________________________________________
bool Tools::LL1_1D_Decision( const unsigned int& arm )
{ return _1D_ll1_decision[arm]; }

//_______________________________________________________
bool Tools::LL1_2D_Decision( const unsigned int& arm )
{ return _2D_ll1_decision[arm]; }

//_______________________________________________________
bool Tools::LL1_1D1S_Decision( const unsigned int& arm )
{ return _1D1S_ll1_decision[arm]; }

