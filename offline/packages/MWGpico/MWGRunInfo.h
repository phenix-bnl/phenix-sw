// $Id: MWGRunInfo.h,v 1.4 2012/02/03 04:45:33 rseidl Exp $

#ifndef RunInfo_h
#define RunInfo_h

/*!
	\file MWGRunInfo.h
	\brief stores run information and nanoDST cuts while processing
  \author  Hugo Pereira
  \version $Revision: 1.4 $
  \date    $Date: 2012/02/03 04:45:33 $
*/

#include <PHInclusiveNanoCuts.h>  
#include <RunHeader.h> 
#include <vector> 

//! stores run information and nanoDST cuts while processing
class MWGRunInfo: public TObject
{
	
	public:
	
	//! constructor
	MWGRunInfo( void ):
		_initialized( false ),
		_pTmin(0), _pTmax(0),
		_vertexcut(0),
		_dimasscut(0),
		_minhitcut(0),
//		_mintrackcut(0), // they seem to be not used anymore
//		_maxtrackcut(0), // and just produce warnings
		_ghostsel(false),
		_dodimu(false)
	{};
	
	//! update RunInfo from run_header and NanoCuts object
	virtual void update( RunHeader* run_header, PHInclusiveNanoCuts  *nano_cuts )
	{
		if( run_header ) _run_number.push_back( run_header->get_RunNumber() );
		if( !_initialized ) initialize( nano_cuts );
		else check_consistancy( nano_cuts );
	}
	
	//! print summary
	virtual void print( void );
		
	protected:
	
	//! initialize cuts from nanoDST cuts object
	virtual void initialize( PHInclusiveNanoCuts* );	
			
	//! check nanoDST cuts consistancy with nanoDST cuts
	virtual void check_consistancy( PHInclusiveNanoCuts* );	
	
	private:
	
	//! true when initialization was done
	bool _initialized;
	
  //! list of runnumbers to which the nanoDST belong
  std::vector<int> _run_number;         
				
	//! nanoDST cut on min transverse momentum
  float _pTmin;
  
  //! nanoDST cut on max transverse momentum
  float _pTmax;
  
  //! nanoDST cut on vertex
  float _vertexcut;
  
  //! nanoDST cut on dimuon mass
  float _dimasscut; 
  
  //! nanoDST cut on min number of hits/track
  int _minhitcut;
  
  //! nanoDST cut on min number of tracks/event
//  int _mintrackcut;
  
  //! nanoDST cut on max number of tracks/event
//  int _maxtrackcut; 
  
  //! true if nanoDST ghost track removal was ON
  bool _ghostsel;
  
  //! true if nanoDST dimuons have been generated
  bool _dodimu;

  //! define class for CINT
  ClassDef(MWGRunInfo,1)
	
};

#endif
