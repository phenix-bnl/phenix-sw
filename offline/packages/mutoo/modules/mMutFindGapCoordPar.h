#ifndef __MMUTFINDGAPCOORDPAR_HH__
#define __MMUTFINDGAPCOORDPAR_HH__

#include<PHObject.h>
#include<MUTOO.h>
#include<TMutParBase.h>
#include<TMutParameterDB.h>

//////////////////////////////////////////////////////////////
/*!
   \file    mMutFindGapCoordPar.h
   \brief   Runtime parameter object for mMutFindGapCoord analysis module
   \author  S. Kelly
   \version $Revision: 1.8 $
   \date    $Date: 2011/12/24 04:48:29 $
*/
//////////////////////////////////////////////////////////////

//!  Runtime parameter object for mMutFindGapCoord analysis module
/*! 
  Runtime parameter object for mMutFindGapCoord analysis module
*/
class mMutFindGapCoordPar : public TMutParBase
{
  
 public: 
  
  /*! default constructor */
  mMutFindGapCoordPar() : 
    _q_match_threshold(1000),
    _q_rel_match_threshold( -1 ),
    _max_anode_dca(0.5)
	{
  		TMutParameterDB::get().get<unsigned short>("mMutFindGapCoord_verbosity", _verbosity );
	}
  
  /*! destructor */
  ~mMutFindGapCoordPar(){;}
  
  /*! Minimum delta Q for match */
  float get_q_match_threshold() const 
  {return _q_match_threshold;} 
  
  /*! Minimum delta Q for match */
  void set_q_match_threshold(float threshold) 
  {_q_match_threshold = threshold;}
  
  /*! Minimum delta Q/Q for match. Negative means no cut */
  float get_q_rel_match_threshold() const 
  {return _q_rel_match_threshold;} 
 
  /*! Minimum delta Q/Q for match */
  void set_q_rel_match_threshold(float threshold) 
  {_q_rel_match_threshold = threshold;}
  
  /*! Maximum distance from gap coord to anode wire */
  float get_max_anode_dca() const 
  {return _max_anode_dca;} 
  
  /*! Maximum distance from gap coord to anode wire */
  void set_max_anode_dca(float max_anode_dca) 
  {_max_anode_dca = max_anode_dca;}
  
  //! dump parameters
  void print( void )
  {
    MUTOO::PRINT( std::cout, "mMutFindGapCoordPar" );
    std::cout << "_q_match_threshold = " << _q_match_threshold << " [charge].\n";
    std::cout << "_q_rel_match_threshold = " << _q_rel_match_threshold << ".\n";
    std::cout << "_max_anode_dca = " << _max_anode_dca << ".\n"; 
  }
  
 private:  
  
  float _q_match_threshold;
  float _q_rel_match_threshold;
  float _max_anode_dca;
  
};

#endif





