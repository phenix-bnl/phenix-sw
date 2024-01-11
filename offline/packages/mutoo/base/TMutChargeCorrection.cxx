// $Id: TMutChargeCorrection.cxx,v 1.3 2007/03/15 23:15:57 hugo Exp $
/*!
   \file    TMutChargeCorrection.cxx
   \brief   Utility class r-q correction and inverse
   \author  H. Pereira
   \version $Revision: 1.3 $
   \date    $Date: 2007/03/15 23:15:57 $
*/

#include "TMutChargeCorrection.h"
#include "TMutKeyGen.h"

// STL
#include<fstream>
#include<iostream>
#include<sstream>
#include<vector>

using namespace std;
ClassImp(TMutChargeCorrection)

//____________________________________________________________
string TMutChargeCorrection::_filename = "charge_corr_param.out";
bool TMutChargeCorrection::_do_correction = true;

//____________________________________________________________
void TMutChargeCorrection::ParameterSet::read( istream &in )
{

  for( int i=0; i< N_PARAM; i++ )
  {
    double param;
    in >> param;
    if( in.rdstate() & ios::failbit ) {
      cout << "TMutChargeCorrection::ParameterSet::read - error reading parameter " << i << endl;
      break;
    }
    
    _params[i] = param;
    
  }

}

//____________________________________________________________
TMutChargeCorrection::ChargeCorrectionMap TMutChargeCorrection::initialize()
{
    
  // clear existing map.
  ChargeCorrectionMap local_map;
  
  // try read file
  ifstream s( _filename.c_str() );
  if( !s ) {
    cout << "TMutChargeCorrection::initialize - using default charge corrections " << endl;
  } else {
  	
    cout << "TMutChargeCorrection::initialize - reading charge corrections from file " << _filename << endl;
    
  	// file found. Reading	
    const int bufsize = 256;
    char linebuf[bufsize];
    while(s.getline(linebuf,bufsize,'\n'))
    {
      
  		istringstream stringbuf(linebuf);
  		
    	// read cathode locator from stream
      int arm,sta,oct,half,gap;
      stringbuf >> arm >> sta >> oct >> half >> gap;
      
      // read parameter set from stream
      ParameterSet params( stringbuf );
      
  		// Get key for this gap location and put into hash-map
      TMutKeyGen::key_type key = TMutKeyGen::get_key(arm, sta, oct, half, gap );
      local_map.insert(make_pair(key,params));
      cout << "TMutChargeCorrection::initialize - read " << local_map.size() << " entries" << endl;
    
    }
    

  }	
  
  MUTOO::PRINT( cout, "**" );

  return local_map;
}

//____________________________________________________________
double TMutChargeCorrection::get_correction(    
    const MUTOO::gap_locator& location,
    const double& x,
    const double& y )
{
  
  if( !_do_correction ) return 0;
  
  int arm( location.get<0>() );
  int station( location.get<1>() );
  int octant( location.get<2>() ); 
  int half( location.get<3>() );
  int gap( location.get<4>() );
  
  // read polynom parameters from map.
  TMutKeyGen::key_type key = TMutKeyGen::get_key(arm, station, octant, half, gap);
  ChargeCorrectionMap::const_iterator iter = get_map().find(key);
  if( iter == get_map().end() ) return 0;
  
  // return calculated correction		
  return iter->second.get_correction( x, y );

}
   
