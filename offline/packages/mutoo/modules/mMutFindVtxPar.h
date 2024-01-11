// $Id: mMutFindVtxPar.h,v 1.5 2005/08/23 10:36:40 hpereira Exp $
#ifndef __MMUTFINDVTXPAR_HH__
#define __MMUTFINDVTXPAR_HH__

//////////////////////////////////////////////////////////////////
/*
  \file mMutFindVtxPar.h
  \brief Runtime parameter object for vertex finding analysis module
  \author S.Kelly 
  \version $Revision: 1.5 $
  \date    $Date: 2005/08/23 10:36:40 $
*/
//////////////////////////////////////////////////////////////////

#include<PHObject.h>
#include<PHException.h>
#include<TMutParBase.h>

//! Runtime parameter object for vertex finding analysis module
class mMutFindVtxPar : public TMutParBase
{  
 public: 

  /*! default constructor */
  mMutFindVtxPar() :
    _use_nagle_fit(false),
    _check_stubs(false)
    {;}
  
  /*! destructor */
  ~mMutFindVtxPar(){;} 
   
  /*! Use nagle fit */ 
  int get_use_nagle_fit( void ) const 
  { return _use_nagle_fit;}
  
  /*! Use nagle fit */ 
  void set_use_nagle_fit( int use_nagle_fit) 
  { _use_nagle_fit=use_nagle_fit;}
  
  //! check stubs
  bool get_check_stubs( void ) const
  { return _check_stubs; }
  
  //! check stubs
  void set_check_stubs( bool value )
  { _check_stubs = value; }
  
 private:
  
  //! parametrized vertex fit 
  bool _use_nagle_fit;
  
  //! check stubs associated to the tracks
  bool _check_stubs;

};

#endif /* __MMUTFINDVTX_HH__ */







