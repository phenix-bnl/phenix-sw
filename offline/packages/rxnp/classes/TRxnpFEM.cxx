// $Id: TRxnpFEM.cxx,v 1.9 2007/05/16 21:21:12 phnxmuid Exp $
//////////////////////////////////////////////////////////////////
/*
        \file TRxnpFEM.cxx
        \brief Implementation of utility class to store and fetch fem 
	\      calibration consts info from database
        \author Chun Zhang
        \version $Revision: 1.9 $
        \date    $Date: 2007/05/16 21:21:12 $
*/
//////////////////////////////////////////////////////////////////

#include <iostream>
#include <fstream>

//#include <boost/thread.hpp>

// PHENIX datebase include
//
#include <PdbBankManager.hh>
#include <PdbApplication.hh>
#include <PdbCalBank.hh>
#include <PdbRxNP.hh>
#include <PdbRxNPSlew.hh>

#include "TRxnpFEMChannel.h"
#include "TRxnpFEM.h"

// Initialize myInstance
TRxnpFEM* TRxnpFEM::dummy_ptr = NULL; 
boost::shared_ptr<TRxnpFEM> TRxnpFEM::myInstance(dummy_ptr);


// constructor
TRxnpFEM::TRxnpFEM()
{
  _run = 0;
  _use_db = true;
  _NBbc = 0;
  //  std::cout << PHWHERE << " Constructor is called"<< std::endl;
}

// destructor
TRxnpFEM::~TRxnpFEM()
{
  // Lock lock(_mutex); 
  _chan_map.clear();
  // std::cout << PHWHERE << " Destructor is called"<< std::endl;
}

//instance
boost::shared_ptr<TRxnpFEM> TRxnpFEM::instance()
{
  if(myInstance.get()==NULL)
    {
      //      Lock lock(_mutex);
      //if(myInstance==NULL)
	// double protection
	myInstance = boost::shared_ptr<TRxnpFEM>(new TRxnpFEM());
    }
  return myInstance;
}

// database

bool TRxnpFEM::fetch()
{
  // clear map
  //
  _chan_map.clear();

  // start database access
  //
  PdbBankManager* bankManager = PdbBankManager::instance();

  PdbApplication *application = bankManager->getApplication();
  if (!application->startRead())
    {
      PHMessage("BunchCrossCal::", PHError, "Aborting ... Database not readable");
      application->abort();
    }

  // General RXNP FEM db  
  //Make a bank ID
  PdbBankID bankID(0);
  PdbCalBank *Bank = bankManager->fetchBank("PdbRxNPBank",
                                            bankID,
                                            "calibrxnpgain",
                                            _run);

  // Slew correction db
  //  Make a bank ID
  PdbBankID bankIDslew(0);
  PdbCalBank *Bankslew = bankManager->fetchBank("PdbRxNPSlewBank",
						bankIDslew,
						"calibrxnpslew",
						_run);
  

  if (Bank&&Bankslew)
    {
      // entry index
      int n = 0;
      int n1 = 0;
      PdbRxNP *rxnp = (PdbRxNP *) & (Bank->getEntry(n));
      PdbRxNPSlew* slew = (PdbRxNPSlew *) & (Bankslew->getEntry(n1));
      for(unsigned short iarm = 0; iarm < RXNP::NARM; iarm++)
        {
          for(unsigned short iring = 0; iring < RXNP::NRING; iring++)
            {
              for(unsigned short iscint = 0; iscint < RXNP::NCHANNEL_PERRING; iscint++)
                {
		  int chann = rxnp->get_channel(iarm, iring, iscint);
		  _chan_list.push_back(chann);
                 // insert a ScintLocation into the _local_map
                  //
                  private_iter iter = (_chan_map.insert(std::make_pair(chann, data_type(new TRxnpFEMChannel(iarm, iring, iscint))))).first;

		  _NBbc=rxnp->get_NBBC();

                  if(iter!=_chan_map.end())
                    {
		      for(int ibin = 0; ibin < _NBbc; ibin++)
                        {
                          (iter->second)->set_high_slop(rxnp->get_HG_ADC_coeff(iarm, iring, iscint, ibin));
                          (iter->second)->set_low_slop(rxnp->get_LG_ADC_coeff(iarm, iring, iscint, ibin));
                        }
                      (iter->second)->set_tdc_slop(rxnp->get_TDC_coeff(iarm, iring, iscint));
		      for(int iadc = 0; iadc < TRxnpFEMChannel::N_ADC; iadc++)
			(iter->second)->set_tdc_lg_slewcoeff(iadc,
							     slew->get_TDC_LG_slewcoeff(iarm, iring, iscint,iadc));
                      for(int iamu = 0; iamu < RXNP::NAMU; iamu++)
                        {
                          (iter->second)->set_high_int(iamu, rxnp->get_hg_ped_mean(iarm,iring, iscint,iamu));
                          (iter->second)->set_low_int(iamu, rxnp->get_lg_ped_mean(iarm,iring, iscint,iamu));
                          (iter->second)->set_tdc_int(iamu, rxnp->get_tdc_ped_mean(iarm,iring, iscint,iamu));
                          (iter->second)->set_high_wid(iamu, rxnp->get_hg_ped_width(iarm,iring, iscint,iamu));
                          (iter->second)->set_low_wid(iamu, rxnp->get_lg_ped_width(iarm,iring, iscint,iamu));
                          (iter->second)->set_tdc_wid(iamu, rxnp->get_tdc_ped_width(iarm,iring, iscint,iamu));
                        }
		    }
		  else
		    {
		      std::cout << PHWHERE << " no space in _chan_map !" << std::endl;
		      delete Bank;
		      delete Bankslew;
		      return false;
		    }
		}
	    }
	}
      delete Bank;
      delete Bankslew;
    }
  else
    {
      std::cout << PHWHERE << "Failed to get RxNP info from DB" << std::endl;
      if(Bank) delete Bank;
      if(Bankslew) delete Bankslew;
      return false;
    }
		    
  return true;
}

bool TRxnpFEM::read()
{
  // for now get the calibration const from a local file.
  //
  std::ifstream fin("rxnp_calib.txt");
  std::ifstream fin2("rxnp_calib_amu.txt");

  if(!fin.is_open())
   {
     std::cout << PHWHERE << " local calibration file does not exist!" << std::endl;
     return false;
   }

  if(!fin2.is_open())
   {
     std::cout << PHWHERE << " local amu calibration file does not exist!" << std::endl;
     return false;
   }

  // clear the map first
  //
  _chan_map.clear();

  _NBbc=1;

  // to avoid read empty last lines
  int lineCount = 0;
  while((!fin.eof())&&lineCount<48)
    {
      int chann, iarm, iring, iscint;
      float high_slop, high_int, low_slop, low_int, high_mip, low_mip;
      float tdc_slop, tdc_int;
      float high_wid, low_wid, tdc_wid;
      
      fin >> chann;
      fin >> iarm;
      fin >> iring;
      fin >> iscint;
      fin >> high_slop;
      fin >> high_int;
      fin >> high_wid;
      fin >> high_mip;
      fin >> low_slop;
      fin >> low_int;
      fin >> low_wid;
      fin >> low_mip;
      fin >> tdc_slop;
      fin >> tdc_int;
      fin >> tdc_wid;
      lineCount++;
      _chan_list.push_back(chann);

      private_iter iter = (_chan_map.insert(std::make_pair(chann, data_type(new TRxnpFEMChannel(iarm, iring, iscint))))).first;
      
      if(iter!=_chan_map.end())
        {
	  (iter->second)->set_high_slop(high_slop);
	  (iter->second)->set_low_slop(low_slop);
	  (iter->second)->set_tdc_slop(tdc_slop);

	  for(int iamu = 0; iamu < RXNP::NAMU; iamu++)
	    {
              int ich,iam;
              fin2 >> ich;
              fin2 >> iam;
              if (chann!=ich || iamu!=iam) {
                std::cout << PHWHERE << " local amu calibration file format error!" << std::endl;
                return false;
              }
              fin2 >> high_int;
              fin2 >> high_wid;
              fin2 >> low_int;
              fin2 >> low_wid;
              fin2 >> tdc_int;
              fin2 >> tdc_wid;
	      (iter->second)->set_high_int(iamu, high_int);
	      (iter->second)->set_low_int(iamu, low_int);
	      (iter->second)->set_tdc_int(iamu, tdc_int);
	      (iter->second)->set_high_wid(iamu, high_wid);
	      (iter->second)->set_low_wid(iamu, low_wid);
	      (iter->second)->set_tdc_wid(iamu, tdc_wid);
	    }
	}
      else
	{
	  std::cout << PHWHERE << " no space in _chan_map !" << std::endl;
	  return false;
	}     
    }
  return true;
}

bool TRxnpFEM::init(int run)
{
  //  Lock lock(_mutex);

  // check if this is a new run, if it is not, no need to access date base, return true.
  if(_run==run)
    return true;
  // copy run number
  //
  _run = run;
  
  bool dbres = false;
  
  if(_use_db) 
    dbres = fetch();
  
  if(!dbres)
    {
      std::cout<<PHWHERE<<" Using local calibration file rxnp_calib.txt!" <<std::endl;
      return read();
    }
  return dbres;
}
