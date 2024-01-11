#include "Tools.h"
#include <MuonUtil.h>
#include <MUTOO.h>
#include <PHCompositeNode.h>

using namespace std;

//_______________________________________________________
boost::array< bool, 2 > Tools::_1S_blt_decision = {{ false }};
boost::array< bool, 2 > Tools::_1D_blt_decision = {{ false }};
boost::array< bool, 2 > Tools::_2D_blt_decision = {{ false }};
boost::array< bool, 2 > Tools::_1D1S_blt_decision = {{ false }};

boost::array< bool, 2 > Tools::_1S_blt_reco_decision = {{ false }};
boost::array< bool, 2 > Tools::_1D_blt_reco_decision = {{ false }};
boost::array< bool, 2 > Tools::_2D_blt_reco_decision = {{ false }};
boost::array< bool, 2 > Tools::_1D1S_blt_reco_decision = {{ false }};

//_______________________________________________________
void Tools::LoadBLTDecision( PHCompositeNode *top_node )
{
  for( unsigned int arm = 0; arm < 2; arm ++ )
  {
    _1S_blt_decision[arm] = MuonUtil::get_1S_BLTtrigger_decision( top_node, arm );
    _1D_blt_decision[arm] = MuonUtil::get_1D_BLTtrigger_decision( top_node, arm );
    _2D_blt_decision[arm] = MuonUtil::get_2D_BLTtrigger_decision( top_node, arm );
    _1D1S_blt_decision[arm] = MuonUtil::get_1D1S_BLTtrigger_decision( top_node, arm );

    _1S_blt_reco_decision[arm] = MuonUtil::get_1S_BLTtrigger_decision( top_node, arm, true );
    _1D_blt_reco_decision[arm] = MuonUtil::get_1D_BLTtrigger_decision( top_node, arm, true );
    _2D_blt_reco_decision[arm] = MuonUtil::get_2D_BLTtrigger_decision( top_node, arm, true );
    _1D1S_blt_reco_decision[arm] = MuonUtil::get_1D1S_BLTtrigger_decision( top_node, arm, true );
  }
}

//_______________________________________________________
bool Tools::BLT_1S_Decision( const unsigned int& arm, const bool& use_reco )
{ return use_reco ? _1S_blt_reco_decision[arm]:_1S_blt_decision[arm]; }

//_______________________________________________________
bool Tools::BLT_1D_Decision( const unsigned int& arm, const bool& use_reco )
{ return use_reco ? _1D_blt_reco_decision[arm]:_1D_blt_decision[arm]; }

//_______________________________________________________
bool Tools::BLT_2D_Decision( const unsigned int& arm, const bool& use_reco )
{ return use_reco ? _2D_blt_reco_decision[arm]:_2D_blt_decision[arm]; }

//_______________________________________________________
bool Tools::BLT_1D1S_Decision( const unsigned int& arm, const bool& use_reco )
{ return use_reco ? _1D1S_blt_reco_decision[arm]:_1D1S_blt_decision[arm]; }
