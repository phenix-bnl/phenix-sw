// $Id: MWGRunInfo.C,v 1.4 2012/02/03 04:45:33 rseidl Exp $

/*!
  \file    MWGRunInfo.C
  \brief   fun4all module to fill Muon picoDSTs from nanoDSTs
  \author  Hugo Pereira
  \version $Revision: 1.4 $
  \date    $Date: 2012/02/03 04:45:33 $
*/

#include "MWGRunInfo.h"
#include <MUTOO.h>
#include <iostream>

using namespace std;

ClassImp( MWGRunInfo );

//______________________________________________________
void MWGRunInfo::initialize( PHInclusiveNanoCuts* nano_cuts )
{
	if( !nano_cuts ) return;
	if( _initialized ) return;
	
	_ghostsel=nano_cuts->get_ghostsel();   
	_dodimu =nano_cuts->get_dodimu();
	_pTmin=nano_cuts->get_ptlowcut();      
	_pTmax=nano_cuts->get_pthighcut();
	_vertexcut=nano_cuts->get_vertexcut(); 
	_dimasscut=nano_cuts->get_dimasscut();
	_minhitcut=nano_cuts->get_minhitcut(); 
//	_mintrackcut=nano_cuts->get_mintrackcut();
//	_maxtrackcut = nano_cuts->get_maxtrackcut();
	_initialized = true;
}
	
//______________________________________________________
void MWGRunInfo::check_consistancy( PHInclusiveNanoCuts* nano_cuts )
{
	
	if( !nano_cuts ) return;
	if( !_initialized ) return;
	if(
			_ghostsel!=nano_cuts->get_ghostsel() || 
			_dodimu!=nano_cuts->get_dodimu() ||
      _pTmin!=nano_cuts->get_ptlowcut() || 
			_pTmax!=nano_cuts->get_pthighcut() ||
      _vertexcut!=nano_cuts->get_vertexcut() || 
			_dimasscut!=nano_cuts->get_dimasscut() ||
      _minhitcut!=nano_cuts->get_minhitcut() 
//|| 					_mintrackcut!=nano_cuts->get_mintrackcut() ||      _maxtrackcut!=nano_cuts->get_maxtrackcut()
) 
	cout<<"MWGRunInfo::check_consistancy - WARNING - inconsistant PHInclusiveNanoCuts."<<endl;
	return;
}

//____________________________________________________________________________
void MWGRunInfo::print()
{
  MUTOO::PRINT( cout, "MWGRunInfo::print" );
	cout << "nanoDSTs were produced with the following RUNS:"<<endl;
  for (unsigned int i=0; i<_run_number.size(); i++) {
		cout << _run_number[i] << " ";
		if (!( (i+1) % 10) ) cout << endl;
  }
	
	if( !_initialized ) cout << endl << "nanoDST cuts could not be retrieved" << endl;	
	cout<<endl<<"nanoDSTs were produced with the following CUTS:"<<endl;
	cout<<" kept events with |Zvtx|<vertexcut            : " << "vertexcut   = " << _vertexcut   <<endl;
//	cout<<" kept events with at least mintrackcut tracks : " << "mintrackcut = " << _mintrackcut <<endl;
//	cout<<" kept events with at most maxtrackcut tracks  : " << "maxtrackcut = " << _maxtrackcut <<endl;    
	cout<<" if 1, kept not ghost tracks                  : " << "ghostsel    = " << _ghostsel    <<endl;
	cout<<" kept tracks with at least minhitcut hits     : " << "minhitcut   = " << _minhitcut   <<endl;
	cout<<" kept tracks with pT>pTmin                    : " << "pTmin       = " << _pTmin       <<endl;
	cout<<" kept tracks with pT<pTmax                    : " << "pTmax       = " << _pTmax       <<endl;
	cout<<" if 1, dimuons branch must have been produced : " << "dodimu      = " << _dodimu      <<endl;
	cout<<" kept dimuons with mass>dimasscut             : " << "dimasscut   = " << _dimasscut   <<endl;
	MUTOO::PRINT( cout, "**" );
	
}
