// $Id: TRxnpGeom.cxx,v 1.5 2007/06/27 00:38:26 phnxbld Exp $
//////////////////////////////////////////////////////////////////
/*
        \file TRxnpGeom.cxx
        \brief Utility class implementation 
        \author Chun Zhang
        \version $Revision: 1.5 $
        \date    $Date: 2007/06/27 00:38:26 $
*/
//////////////////////////////////////////////////////////////////

#include <iostream>
#include <fstream>
#include <cmath>

// PHENIX datebase include
//
#include <PdbBankManager.hh>
#include <PdbApplication.hh>
#include <PdbCalBank.hh>
#include <PdbRxNPSurvey.hh>

#include "TRxnpScintGeom.h"
#include "TRxnpGeom.h"

// Initialize myInstance
TRxnpGeom* TRxnpGeom::dummy_ptr = NULL; 
boost::shared_ptr<TRxnpGeom> TRxnpGeom::myInstance(dummy_ptr);

// constructor
TRxnpGeom::TRxnpGeom()
{
  for(int arm = 0; arm < RXNP::NARM; arm++)
    {
      for(int ring = 0; ring < RXNP::NRING; ring++)
	{
	  for(int scint = 0; scint < RXNP::NCHANNEL_PERRING; scint++)
	    {
	      _scint[arm][ring][scint]= new TRxnpScintGeom(arm, ring, scint); 
	    }
	}
    }
  _use_db = true;
}
//destructor
TRxnpGeom::~TRxnpGeom()
{
  //  Lock lock(_mutex);
    for(int arm = 0; arm < RXNP::NARM; arm++)
    {
      for(int ring = 0; ring < RXNP::NRING; ring++)
	{
	  for(int scint = 0; scint < RXNP::NCHANNEL_PERRING; scint++)
	    {
	      if(_scint[arm][ring][scint]) 
		delete _scint[arm][ring][scint];
	    }
	}
    }
}
//instance
boost::shared_ptr<TRxnpGeom> TRxnpGeom::instance()
{
  if(myInstance.get()==NULL)
    {
      //    Lock lock(_mutex);
      //      if(myInstance==NULL)
	// double protection
	myInstance = boost::shared_ptr<TRxnpGeom>(new TRxnpGeom());
    }
  return myInstance;
}

// database
bool TRxnpGeom::fetch()
{

  // database access
  //
  PdbBankManager* bankManager = PdbBankManager::instance();
  
  PdbApplication *application = bankManager->getApplication();
  if (!application->startRead())
    {
      PHMessage("BunchCrossCal::", PHError, "Aborting ... Database not readable");
      application->abort();
    }
  
  //  Make a bank ID
  PdbBankID bankID(0);
  PdbCalBank *Bank = bankManager->fetchBank("PdbRxNPSurveyBank",
                                            bankID,
                                            "calibrxnpgeom",
                                            _t);
  
  if (Bank)
    {
      PdbRxNPSurvey *rxnp = (PdbRxNPSurvey *) & (Bank->getEntry(0));
     
      for(int arm = 0; arm < RXNP::NARM; arm++)
	{
	  for(int scint = 0; scint < RXNP::NCHANNEL_PERRING; scint++)
	    {
	      for(int isvy = 0; isvy < TRxnpScintGeom::NSURVEYPOSITION; isvy++)
		{
		  // xie wei betried the convention, so a transformation is need.
		  //
		  float x = rxnp->get_X(arm,scint,isvy);
		  float y = rxnp->get_Y(arm,scint,isvy);
		  float z = rxnp->get_Z(arm,scint,isvy);
		  float theta = std::atan2(std::sqrt(x*x+y*y), z);
		  float phi = std::atan2(y,x);
		  
		  switch(isvy)
		    {
		    case 0:
		      set_phi_at(arm, 1, scint, TRxnpScintGeom::OUTER_RIGHT, phi);
		      set_theta_at(arm, 1, scint, TRxnpScintGeom::OUTER_RIGHT, theta);
		      set_z_at(arm, 1, scint, TRxnpScintGeom::OUTER_RIGHT, z);
		      break;
		    case 1:
		      set_phi_at(arm, 1, scint, TRxnpScintGeom::MIDDLE_RIGHT, phi);
		      set_theta_at(arm, 1, scint, TRxnpScintGeom::MIDDLE_RIGHT, theta);
		      set_z_at(arm, 1, scint, TRxnpScintGeom::MIDDLE_RIGHT, z);
		      set_phi_at(arm, 0, scint, TRxnpScintGeom::MIDDLE_RIGHT, phi);
		      set_theta_at(arm, 0, scint, TRxnpScintGeom::MIDDLE_RIGHT, theta);
		      set_z_at(arm, 0, scint, TRxnpScintGeom::MIDDLE_RIGHT, z);
		      break;
		    case 2:
		      set_phi_at(arm, 1, scint, TRxnpScintGeom::MIDDLE_LEFT, phi);
		      set_theta_at(arm, 1, scint, TRxnpScintGeom::MIDDLE_LEFT, theta);
		      set_z_at(arm, 1, scint, TRxnpScintGeom::MIDDLE_LEFT, z);
		      set_phi_at(arm, 0, scint, TRxnpScintGeom::MIDDLE_LEFT, phi);
		      set_theta_at(arm, 0, scint, TRxnpScintGeom::MIDDLE_LEFT, theta);
		      set_z_at(arm, 0, scint, TRxnpScintGeom::MIDDLE_LEFT, z);
		      break;
		    case 3:
		      set_phi_at(arm, 1, scint, TRxnpScintGeom::OUTER_LEFT, phi);
		      set_theta_at(arm, 1, scint, TRxnpScintGeom::OUTER_LEFT, theta);
		      set_z_at(arm, 1, scint, TRxnpScintGeom::OUTER_LEFT, z);
		      break;
		    case 4:
		      set_phi_at(arm, 0, scint, TRxnpScintGeom::INNER_RIGHT, phi);
		      set_theta_at(arm, 0, scint, TRxnpScintGeom::INNER_RIGHT, theta);
		      set_z_at(arm, 0, scint, TRxnpScintGeom::INNER_RIGHT, z);
		      break;
		    case 5:
		      set_phi_at(arm, 0, scint, TRxnpScintGeom::INNER_LEFT, phi);
		      set_theta_at(arm, 0, scint, TRxnpScintGeom::INNER_LEFT, theta);
		      set_z_at(arm, 0, scint, TRxnpScintGeom::INNER_LEFT, z);
		      break;
		    }
		}
	    }
	}  
      delete Bank;
    }
  else
    {
      std::cout << PHWHERE << "Failed to get RxNP info from DB" << std::endl;
      return false;
    }
  return true;
}

// local file
bool TRxnpGeom::read()
{

  // Init the TRxnpGeom from a local file, in case db fails
  // 
  //
  std::ifstream fin("rxnp_geom.txt");

  if(!fin.is_open())
    {
      std::cout<<PHWHERE<<"Geometry file rxnp_geom.txt does not exist in local directory.!" <<std::endl;
      return false;
    }
  // to avoid read empty last lines
  int lineCount = 0;
  while((!fin.eof())&&lineCount<48)
    {
      int ichann;
      unsigned short arm, ring, scint;
      float phi, theta;

      fin>>ichann;
      fin>>arm;
      fin>>ring;
      fin>>scint;
      fin>>phi;
      fin>>theta;
      lineCount++;
      for(int isvy = 0; isvy < TRxnpScintGeom::NSURVEYPOSITION; isvy++)
	{
	  set_phi_at(arm, ring, scint, TRxnpScintGeom::OUTER_RIGHT, phi);
	  set_theta_at(arm, ring, scint, TRxnpScintGeom::OUTER_RIGHT, theta);
	  set_phi_at(arm, ring, scint, TRxnpScintGeom::OUTER_LEFT, phi);
	  set_theta_at(arm, ring, scint, TRxnpScintGeom::OUTER_LEFT, theta);

	  set_phi_at(arm, ring, scint, TRxnpScintGeom::MIDDLE_RIGHT, phi);
	  set_theta_at(arm, ring, scint, TRxnpScintGeom::MIDDLE_RIGHT, theta);
	  set_phi_at(arm, ring, scint, TRxnpScintGeom::MIDDLE_LEFT, phi);
	  set_theta_at(arm, ring, scint, TRxnpScintGeom::MIDDLE_LEFT, theta);

	  set_phi_at(arm, ring, scint, TRxnpScintGeom::INNER_RIGHT, phi);
	  set_theta_at(arm, ring, scint, TRxnpScintGeom::INNER_RIGHT, theta);
	  set_phi_at(arm, ring, scint, TRxnpScintGeom::INNER_LEFT, phi);
	  set_theta_at(arm, ring, scint, TRxnpScintGeom::INNER_LEFT, theta);
	  //	  set_z_at(arm, ring, scint, TRxnpScintGeom::OUTER_RIGHT, z);      
	}
    }
  fin.close();
  return true;
}

// init
bool TRxnpGeom::init(const PHTimeStamp& T)    
{
  //  Lock lock(_mutex);

  // check if this is a new run, if it is not, need to access date base, return true
  //
  if(_t == T)
    return true;
  // copy the time stamp
  //
  _t = T;

  bool db_res = false;
  if(_use_db)
    db_res = fetch();

  if(!db_res)
    {
      std::cout<<PHWHERE<<" Using local geometry file rxnp_geom.txt!" <<std::endl;
      return read();
    }
  return true;
}

