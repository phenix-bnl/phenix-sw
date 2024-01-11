#include "ReactionPlaneCalib.h"

#include <PdbBankManager.hh>
#include <PdbApplication.hh>
#include <PdbParameter.hh>
#include <PdbCalBank.hh>
#include <RunToTime.hh>
#include "PHTimeStamp.h"

#include <RunNumberRanges.h>
#include <utiCentrality.h>
#include <PHGlobal.h>
#include <RunHeader.h>

#include <getClass.h>

#include <cstdlib>
#include <fstream>
#include <iostream>
#include <sstream>

using namespace RP;
using namespace std;


ReactionPlaneCalib::ReactionPlaneCalib()
{
  Reset();

  verbosity = 0;
  RunNumber = 0;

  databasename = "calib.reactionplane";
}

void ReactionPlaneCalib::Reset()
{


  for (int id = 0;id < NDET;id++) {
    for (int ih = 0;ih < NHAR;ih++) {
      for (int im = 0;im < NMUL;im++) {
	SumXmean[id][ih][im]  = 0.0;
	SumXsigma[id][ih][im] = 1.0;
	SumYmean[id][ih][im]  = 0.0;
	SumYsigma[id][ih][im] = 1.0;
      }
    }
  }

  for (int id = 0;id < NDET;id++) {
    for (int ih = 0;ih < NHAR;ih++) {
      for (int im = 0;im < NMUL;im++) {
        for (int iz = 0;iz < NZPS;iz++) {
          for (int io = 0;io < NORD;io++) {
	    FlatCos[id][ih][im][iz][io] = 0.0;
	    FlatSin[id][ih][im][iz][io] = 0.0;
	  }
	}
      }
    }
  }

  for(int id=NDET; id<NDET2; id++) {
    for(int ih=0; ih<NHAR2; ih++) {
      for(int im=0; im<NMUL2; im++) {
        SumXmean2[id-NDET][ih][im]  = 0.0;
        SumXsigma2[id-NDET][ih][im] = 1.0;
        SumYmean2[id-NDET][ih][im]  = 0.0;
        SumYsigma2[id-NDET][ih][im] = 1.0;
      }
    }
  }

  for(int id=NDET; id<NDET2; id++) {
    for(int ih=0; ih<NHAR2; ih++) {
      for(int im=0; im<NMUL2; im++) {
       	for(int iz=0; iz<NZPS2; iz++) {
	  for(int io=0; io<NORD2; io++) {
            FlatCos2[id-NDET][ih][im][iz][io] = 0.0;
            FlatSin2[id-NDET][ih][im][iz][io] = 0.0;
	  }
	}
      }
    }
  }

}

float ReactionPlaneCalib::Flattening(const int idet, const int ihar, const int imul, 
                                     const int izps, const float psi)
{
  float deltaPsi = 0.0;
  float psi0 = psi * (ihar + 1.0); // -pi < psi0 < pi

  if(idet<NDET) {
//    for (int io = 0;io < NORD;io++) {
    for (int io = 0;io < 5;io++) {
      float averageCos = FlatCos[idet][ihar][imul][izps][io];
      float averageSin = FlatSin[idet][ihar][imul][izps][io];

      float cosPsi = cos( (io + 1.0) * psi0 );
      float sinPsi = sin( (io + 1.0) * psi0 );
      deltaPsi += ( -averageSin * cosPsi + averageCos * sinPsi) * 2.0 / (io + 1.0);
    }
  }else{
//    for (int io = 0;io < NORD2;io++) {
    for (int io = 0;io < 5;io++) {
      float averageCos = FlatCos2[idet-NDET][ihar][imul][izps][io];
      float averageSin = FlatSin2[idet-NDET][ihar][imul][izps][io];

      float cosPsi = cos( (io + 1.0) * psi0 );
      float sinPsi = sin( (io + 1.0) * psi0 );
      deltaPsi += ( -averageSin * cosPsi + averageCos * sinPsi) * 2.0 / (io + 1.0);
    } 
  }

  return atan2(sin(psi0 + deltaPsi), cos(psi0 + deltaPsi)) / (ihar + 1.0); // -pi/n < psi < pi/n
}

int ReactionPlaneCalib::Fetch(const int runNumber)
{
  RunNumber = runNumber;
  
  cout << "ReactionPlaneCalib::Fetch from DB for run " << RunNumber << endl;

  // DLW version: check env var for filename template.  This template
  // is expected to have one %d field in it, and it will be replaced with
  // the current run number.
  if ( char* fnameTemplate = getenv("RPCALIB_FILENAME_TEMPLATE") )
    {
      const int LEN = 4096;
      char fname[LEN];
      int n = snprintf(fname,LEN,fnameTemplate,runNumber);
      if ( n >= LEN ) 
	{
	  std::cout << "ReactionPlaneCalib::Fetch: WARNING: snprintf truncated output string" 
		    << std::endl;
	}
      std::cout << "ReactionPlaneCalib::Fetch: overriding DB fetch with file " << fname << std::endl;

      // Attempt to use the provided file.  If successful, return.  If not,
      // proceed with DB fetching and see if that works.
      if ( Fetch(fname) == 0 ) return 0;
      else
	std::cout << "ReactionPlaneCalib::Fetch: Failed to fetch calib from file, "
		  << "trying DB instead... " << std::endl;
    }

  PdbBankManager* bankManager = PdbBankManager::instance();
  PdbApplication* application = bankManager->getApplication();

  if (!application->startRead())
    {
      cout << PHWHERE << " Error : Aborting ... Database not readable" << endl;
      application->abort();
    }

  // make bank ID
  PdbBankID bankID;
  bankID.setInternalValue(GetBankID());

  // Grap a pointer to the bank ...
  PdbCalBank* rpBank = bankManager->fetchBank("PdbParameterBank", bankID, databasename.c_str(), runNumber);
  if (!rpBank)
    {
      return -1;
    }
  Fetch(rpBank);
  return 0;
}

void ReactionPlaneCalib::Fetch(const PHTimeStamp& tstart)
{
  cout << "ReactionPlaneCalib::Fetch from DB." << endl;

  PdbBankManager* bankManager = PdbBankManager::instance();
  PdbApplication* application = bankManager->getApplication();

  if (!application->startRead())
    {
      cout << PHWHERE << " Error : Aborting ... Database not readable" << endl;
      application->abort();
    }

  // Set run number from time stamp
  RunNumber = RunToTime::instance()->getRunNumber(tstart);

  // make bank ID
  PdbBankID bankID;
  bankID.setInternalValue(GetBankID());

  // Grap a pointer to the bank ...
  PHTimeStamp Tstart = tstart;
  PdbCalBank* rpBank = bankManager->fetchBank("PdbParameterBank", bankID, databasename.c_str(), Tstart);

  Fetch(rpBank);
  return ;
}

void ReactionPlaneCalib::Fetch(PdbCalBank* rpBank)
{
  //----------------------------------------------------
  //  OK...now is the time to actually unpack the data...
  //  three checks...length of record, scheme and no. entries...
  const int bankid = rpBank->getBankID().getInternalValue() ;
  const int length = GetDBLength(bankid);

  int truelength = rpBank->getLength();

  if (length != truelength) {
    cout << PHWHERE << " FATAL...wrong length DB read for R.P." << endl;
    return ;
  }

  int index = 0;
  PdbParameter* parameter = (PdbParameter*)(& rpBank->getEntry(index++));
  int scheme = (int)parameter->getParameter();
  if (scheme != 1) {
    cout << PHWHERE << " FATAL...wrong scheme DB read for R.P." << endl;
    return ;
  }

  parameter = (PdbParameter *)(& rpBank->getEntry(index++));
  int entries = (int)parameter->getParameter();
  if (entries != length - 2) {
    cout << PHWHERE << " FATAL...wrong entries DB read for R.P" << endl;
    return ;
  }

  //----------------------------------------------------
  //  Checks passed...get the parameters...
  cout << "READ from Database: Reaction Plane" << endl;

  cout << " read SumX/Y correction" << endl;
  for (int idet = 0;idet < NDET;idet++) {
    for (int ihar = 0;ihar < NHAR;ihar++) {
      for (int imul = 0;imul < NMUL;imul++) {
        parameter = (PdbParameter*) & rpBank->getEntry(index++);
        SumXmean[idet][ihar][imul] = parameter->getParameter();

        parameter = (PdbParameter*) & rpBank->getEntry(index++);
        SumXsigma[idet][ihar][imul] = parameter->getParameter();

        parameter = (PdbParameter*) & rpBank->getEntry(index++);
        SumYmean[idet][ihar][imul] = parameter->getParameter();

        parameter = (PdbParameter*) & rpBank->getEntry(index++);
        SumYsigma[idet][ihar][imul] = parameter->getParameter();

        if (verbosity > 2) {
          cout << "X mean " << SumXmean[idet][ihar][imul]
               << " sigma " << SumXsigma[idet][ihar][imul]
               << " Y mean " << SumYmean[idet][ihar][imul]
               << " sigma " << SumYsigma[idet][ihar][imul]
               << endl;
        }
      }
    }
  }

  cout << " read flattening correction ... " << endl;
  for (int id = 0;id < NDET;id++) {
    for (int ih = 0;ih < NHAR;ih++) {
      for (int im = 0;im < NMUL;im++) {
        for (int iz = 0;iz < NZPS;iz++) {
          for (int io = 0;io < NORD;io++) {
            parameter = (PdbParameter*) & rpBank->getEntry(index++);
            FlatCos[id][ih][im][iz][io] = parameter->getParameter();

            parameter = (PdbParameter*) & rpBank->getEntry(index++);
            FlatSin[id][ih][im][iz][io] = parameter->getParameter();

            if (verbosity > 2) {
              cout << "cos " << FlatCos[id][ih][im][iz][io]
                   << " sin " << FlatSin[id][ih][im][iz][io]
                   << endl;
            }
          }
        }
      }
    }
  }


  // RXN and MPC
  if( bankid == 2 ){

    cout << " read SumX/Y correction for DET2" << endl;
    for (int idet=NDET; idet<NDET2; idet++) { //NDET2=29
      for (int ihar=0; ihar<NHAR2; ihar++) {
        for (int imul=0; imul<NMUL2; imul++) {
          parameter = (PdbParameter*) & rpBank->getEntry(index++);
          SumXmean2[idet-NDET][ihar][imul] = parameter->getParameter();
 
          parameter = (PdbParameter*) & rpBank->getEntry(index++);
          SumXsigma2[idet-NDET][ihar][imul] = parameter->getParameter();
 
          parameter = (PdbParameter*) & rpBank->getEntry(index++);
          SumYmean2[idet-NDET][ihar][imul] = parameter->getParameter();
 
          parameter = (PdbParameter*) & rpBank->getEntry(index++);
          SumYsigma2[idet-NDET][ihar][imul] = parameter->getParameter();
 
          if (verbosity > 2) {
            cout << "X mean " << SumXmean2[idet-NDET][ihar][imul]
                 << " sigma " << SumXsigma2[idet-NDET][ihar][imul]
                 << " Y mean " << SumYmean2[idet-NDET][ihar][imul]
                 << " sigma " << SumYsigma2[idet-NDET][ihar][imul]
                 << endl;
          }
        }
      }
    }

    cout << " read flattening correction for DET2" << endl;
    for (int idet=NDET; idet<NDET2; idet++) { //NDET2=29
      for (int ihar=0; ihar<NHAR2; ihar++) {
        for (int imul=0; imul<NMUL2; imul++) {
          for (int iz=0; iz<NZPS2; iz++) {
            for (int io=0; io<NORD2; io++) {
              parameter = (PdbParameter*) & rpBank->getEntry(index++);
              FlatCos2[idet-NDET][ihar][imul][iz][io] = parameter->getParameter();

              parameter = (PdbParameter*) & rpBank->getEntry(index++);
              FlatSin2[idet-NDET][ihar][imul][iz][io] = parameter->getParameter();

              if (verbosity > 2) {
               cout << " cos " << FlatCos2[idet-NDET][ihar][imul][iz][io]
                    << " sin " << FlatSin2[idet-NDET][ihar][imul][iz][io]
                    << endl;
              }
            }
          }
        }
      }
    }

  }

  delete rpBank;
}

int
ReactionPlaneCalib::Fetch(const char* filename)
{
  // Need to set run number before calling this function

  ifstream file(filename);
  if (!file) {
    cout << PHWHERE << " Could not open input file " << filename << endl;
    return -1;
  }

  if (verbosity > 0) {
    cout << "ReactionPlaneCalib:: R.P. calibration parameter from " << filename << endl;
  }

  // SumX/Y correction
  cout << " read SumX/Y correction for BBC/SMD/FCL/CNT" << endl;
  for (int idet = 0;idet < NDET;idet++) {
    for (int ihar = 0;ihar < NHAR;ihar++) {
      for (int imul = 0;imul < NMUL;imul++) {
        file >> SumXmean[idet][ihar][imul] >> SumXsigma[idet][ihar][imul]
             >> SumYmean[idet][ihar][imul] >> SumYsigma[idet][ihar][imul];

        if (verbosity > 2) {
          cout << "X mean " << SumXmean[idet][ihar][imul]
               << " sigma " << SumXsigma[idet][ihar][imul]
               << " Y mean " << SumYmean[idet][ihar][imul]
               << " sigma " << SumYsigma[idet][ihar][imul]
               << endl;
        }
      }
    }
  }

  // flattening correction
  cout << " read flattening correction  for BBC/SMD/FCL/CNT " << endl;
  for (int id = 0;id < NDET;id++) {
    for (int ih = 0;ih < NHAR;ih++) {
      for (int im = 0;im < NMUL;im++) {
        for (int iz = 0;iz < NZPS;iz++) {
          for (int io = 0;io < NORD;io++) {
            file >> FlatCos[id][ih][im][iz][io] >> FlatSin[id][ih][im][iz][io];
            if (verbosity > 2) {
              cout << "cos " << FlatCos[id][ih][im][iz][io]
                   << " sin " << FlatSin[id][ih][im][iz][io]
                   << endl;
            }
          }
        }
      }
    }
  }


  if( RunNumber > BEGIN_OF_RUN7 ){
    cout << " read SumX/Y correction for RXN/MPC" << endl;

    for (int idet=NDET; idet<NDET2; idet++) { //NDET2=29
      for (int ihar=0; ihar<NHAR2; ihar++) {
       	for (int imul=0; imul<NMUL2; imul++) {
	  file >> SumXmean2[idet-NDET][ihar][imul]
               >> SumXsigma2[idet-NDET][ihar][imul]
               >> SumYmean2[idet-NDET][ihar][imul] 
               >> SumYsigma2[idet-NDET][ihar][imul];

          if (verbosity > 2) {
            cout << "X mean " << SumXmean2[idet-NDET][ihar][imul]
                 << " sigma " << SumXsigma2[idet-NDET][ihar][imul]
                 << " Y mean " << SumYmean2[idet-NDET][ihar][imul]
                 << " sigma " << SumYsigma2[idet-NDET][ihar][imul]
                 << endl;
          }
	}
      }
    }

    cout << " read flattening correction for RXN/MPC" << endl;
    for (int idet=NDET; idet<NDET2; idet++) { //NDET2=29
      for (int ihar=0; ihar<NHAR2; ihar++) {
       	for (int imul=0; imul<NMUL2; imul++) {
	  for (int iz=0; iz<NZPS2; iz++) {
	    for (int io=0; io<NORD2; io++) {
	      file >> FlatCos2[idet-NDET][ihar][imul][iz][io]
                   >> FlatSin2[idet-NDET][ihar][imul][iz][io];

              if (verbosity > 2) {
                cout << "cos " << FlatCos2[idet-NDET][ihar][imul][iz][io]
                     << " sin " << FlatSin2[idet-NDET][ihar][imul][iz][io]
                     << endl;
              }
	    }
	  }
	}
      }
    }

  }

  file.close();

  return 0;
}

void ReactionPlaneCalib::Update(const int beginrun, const int endrun)
{
  RunNumber = beginrun;

  RunToTime* runTime = RunToTime::instance();
  PHTimeStamp *ts(runTime->getBeginTime(beginrun));
  PHTimeStamp Tstart = *ts;
  delete ts;
  PHTimeStamp Tstop;

  // The runnumber is encoded into PHTimeStamp.
  if (endrun > 0) {
    ts = runTime->getEndTime(endrun);
    Tstop = *ts;
    delete ts;
  }
  else {
    Tstop.setToFarFuture();
  }

  //  Make the managers...
  PdbBankManager* bankManager = PdbBankManager::instance();
  PdbApplication* application = bankManager->getApplication();

  if (!application->startUpdate()) {
    cout << PHWHERE << " Error : Aborting ... Database not writable" << endl;
    application->abort();
  }

  //  Make a bank ID...
  PdbBankID bankID;
  bankID.setInternalValue(GetBankID());

  ostringstream descrip;
  descrip << "R.P. parameters for RUN " << RunNumber;

  //  Grap a pointer to the bank...
  PdbCalBank* rpBank = bankManager->createBank("PdbParameterBank", bankID, descrip.str().c_str(), Tstart, Tstop, databasename.c_str());

  return Update(rpBank, application);
}

void ReactionPlaneCalib::Update(const PHTimeStamp& tstart, const PHTimeStamp& tstop)
{
  //  Make the managers...
  PdbBankManager* bankManager = PdbBankManager::instance();
  PdbApplication* application = bankManager->getApplication();

  if (!application->startUpdate()) {
    cout << PHWHERE << " Error : Aborting ... Database not writable" << endl;
    application->abort();
  }

  // Get run number from time stamp
  RunNumber = RunToTime::instance()->getRunNumber(tstart);

  //  Make a bank ID...
  PdbBankID bankID;
  bankID.setInternalValue(GetBankID());
  ostringstream descrip;
  descrip << "R.P. parameters for RUN " << RunNumber;

  PHTimeStamp Tstart = tstart;
  PHTimeStamp Tstop = tstop;

  //  Grap a pointer to the bank...
  PdbCalBank* rpBank = bankManager->createBank("PdbParameterBank", bankID, descrip.str().c_str(), Tstart, Tstop, databasename.c_str());

  return Update(rpBank, application);
}

void ReactionPlaneCalib::Update(PdbCalBank* rpBank, PdbApplication* application)
{

  const int bankid = rpBank->getBankID().getInternalValue() ;
  const int length = GetDBLength( bankid );
  rpBank->setLength(length);

  cout << "Length of DB = " << length << endl;


  PdbParameter *parameter;
  int index = 0;
  parameter = (PdbParameter *) & rpBank->getEntry(index++);
  parameter->setParameter(1.0);
  parameter->setName("scheme");

  parameter = (PdbParameter *) & rpBank->getEntry(index++);
  parameter->setParameter(length - 2);
  parameter->setName("entries");

  if (verbosity > 0){
    cout << " Set parameter for BBC/SMD/MVD/FCL/CNT R.P." << endl;
  }

  for (int idet = 0;idet < NDET;idet++) {
    for (int ihar = 0;ihar < NHAR;ihar++) {
      for (int imul = 0;imul < NMUL;imul++) {
        parameter = (PdbParameter*) & rpBank->getEntry(index++);
        parameter->setParameter(SumXmean[idet][ihar][imul]);
        parameter->setName("sumXmean");

        parameter = (PdbParameter*) & rpBank->getEntry(index++);
        parameter->setParameter(SumXsigma[idet][ihar][imul]);
        parameter->setName("sumXsigma");

        parameter = (PdbParameter*) & rpBank->getEntry(index++);
        parameter->setParameter(SumYmean[idet][ihar][imul]);
        parameter->setName("sumYmean");

        parameter = (PdbParameter*) & rpBank->getEntry(index++);
        parameter->setParameter(SumYsigma[idet][ihar][imul]);
        parameter->setName("sumYsigma");
      }
    }
  }

  if (verbosity > 0) {
    cout << " set flattening correction ... " << endl;
  }

  for (int id = 0;id < NDET;id++) {
    for (int ih = 0;ih < NHAR;ih++) {
      for (int im = 0;im < NMUL;im++) {
        for (int iz = 0;iz < NZPS;iz++) {
          for (int io = 0;io < NORD;io++) {
            parameter = (PdbParameter*) & rpBank->getEntry(index++);
            parameter->setParameter(FlatCos[id][ih][im][iz][io]);
            parameter->setName("flatCos");

            parameter = (PdbParameter*) & rpBank->getEntry(index++);
            parameter->setParameter(FlatSin[id][ih][im][iz][io]);
            parameter->setName("flatSin");
          }
        }
      }
    }
  }


  if( bankid == 2 ){
    if(verbosity>0){cout << " Set parameter andflattening correction for RXN MPC" << endl;}

    for (int idet=NDET; idet<NDET2; idet++) { //NDET2=29
      for (int ihar=0; ihar<NHAR2; ihar++) {
        for (int imul=0; imul<NMUL2; imul++) {
          parameter = (PdbParameter*) & rpBank->getEntry(index++);
          parameter->setParameter(SumXmean2[idet-NDET][ihar][imul]);
          parameter->setName("sumXmean");
 
          parameter = (PdbParameter*) & rpBank->getEntry(index++);
          parameter->setParameter(SumXsigma2[idet-NDET][ihar][imul]);
          parameter->setName("sumXsigma");
 
          parameter = (PdbParameter*) & rpBank->getEntry(index++);
          parameter->setParameter(SumYmean2[idet-NDET][ihar][imul]);
          parameter->setName("sumYmean");
 
          parameter = (PdbParameter*) & rpBank->getEntry(index++);
          parameter->setParameter(SumYsigma2[idet-NDET][ihar][imul]);
          parameter->setName("sumYsigma");
        }
      }
    }
 
    for (int idet=NDET; idet<NDET2; idet++) { //NDET2=29
      for (int ihar=0; ihar<NHAR2; ihar++) {
        for (int imul=0; imul<NMUL2; imul++) {
          for (int iz=0; iz<NZPS2; iz++) {
            for (int io=0; io<NORD2; io++) {
              parameter = (PdbParameter*) & rpBank->getEntry(index++);
              parameter->setParameter(FlatCos2[idet-NDET][ihar][imul][iz][io]);
              parameter->setName("flatCos");
 
              parameter = (PdbParameter*) & rpBank->getEntry(index++);
              parameter->setParameter(FlatSin2[idet-NDET][ihar][imul][iz][io]);
              parameter->setName("flatSin");
            }
          }
        }
      }
    }

  }

  if (verbosity>0){
    cout << "ReactionPlaneCalib:: Update parameter " << endl;
  }

  application->commit(rpBank);
}

void ReactionPlaneCalib::Write(const char* filename)
{
  // Need to set run number before calling Write() function

  cout << "Write parameter to " << filename << endl;

  ofstream fout(filename);

  for(int id=0;id<NDET;id++){
    for(int ih=0;ih<NHAR;ih++){
      for(int im=0;im<NMUL;im++){
        fout << SumXmean[id][ih][im] << "  " << SumXsigma[id][ih][im] << "  "
             << SumYmean[id][ih][im] << "  " << SumYsigma[id][ih][im]
             << endl;
      }
    }
  }
  for (int id = 0;id < NDET;id++) {
    for (int ih = 0;ih < NHAR;ih++) {
      for (int im = 0;im < NMUL;im++) {
        for (int iz = 0;iz < NZPS;iz++) {
          for (int io = 0;io < NORD;io++) {
            fout << FlatCos[id][ih][im][iz][io] << "  " << FlatSin[id][ih][im][iz][io] << endl;
	  }
	}
      }
    }
  }


  if( RunNumber > BEGIN_OF_RUN7 ){

    for(int id=NDET; id<NDET2; id++) {
     for(int ih=0; ih<NHAR2; ih++) {
      for(int im=0; im<NMUL2; im++) {
          fout << SumXmean2[id-NDET][ih][im] << "  " << SumXsigma2[id-NDET][ih][im] << "  "
               << SumYmean2[id-NDET][ih][im] << "  " << SumYsigma2[id-NDET][ih][im]
               << endl;
        }
      }
    }
 
    for (int id = NDET;id < NDET2;id++) {
      for (int ih = 0;ih < NHAR2;ih++) {
        for (int im = 0;im < NMUL2;im++) {
          for (int iz = 0;iz < NZPS2;iz++) {
            for (int io = 0;io < NORD2;io++) {
              fout << FlatCos2[id-NDET][ih][im][iz][io] << "  " << FlatSin2[id-NDET][ih][im][iz][io] << endl;
            }
          }
        }
      }
    }

  }

  fout.close();

  cout << "Write parameter to " << filename << " [DONE] " << endl;
}

float ReactionPlaneCalib::GetSumXmean(const int idet, const int ihar, const int imul)
{
 if(idet<NDET) {
  if(idet<0 || idet>NDET-1 || ihar<0 || ihar>NHAR-1 || imul<0 || imul>NMUL-1) {
    if(verbosity>1){
      cout << PHWHERE << " Error : check index (id,ih,im)=("
           << idet << "," << ihar << "," << imul << ")" << endl;
    }

    return 0.0;
  }
  return SumXmean[idet][ihar][imul];
 }else{
  if(idet<0 || idet>NDET2-1 || ihar<0 || ihar>NHAR2-1 || imul<0 || imul>NMUL2-1) {
    if(verbosity>1){
      cout << PHWHERE << " Error : check index2 (id,ih,im)=("
           << idet << "," << ihar << "," << imul << ")" << endl;
    }

    return 0.0;
  }
  return SumXmean2[idet-NDET][ihar][imul];
 }
}

float ReactionPlaneCalib::GetSumXsigma(const int idet, const int ihar, const int imul)
{
 if(idet<NDET) {
  if(idet<0 || idet>NDET-1 || ihar<0 || ihar>NHAR-1 || imul<0 || imul>NMUL-1) {
    if(verbosity>1){
      cout << PHWHERE << " Error : check index (id,ih,im)=("
           << idet << "," << ihar << "," << imul << ")" << endl;
    }
    return 1.0;
  }
  return SumXsigma[idet][ihar][imul];
 }else{
  if(idet<0 || idet>NDET2-1 || ihar<0 || ihar>NHAR2-1 || imul<0 || imul>NMUL2-1) {
    if(verbosity>1){
      cout << PHWHERE << " Error : check index2 (id,ih,im)=("
           << idet << "," << ihar << "," << imul << ")" << endl;
    }
    return 1.0;
  }
  return SumXsigma2[idet-NDET][ihar][imul];
 }
}

float ReactionPlaneCalib::GetSumYmean(const int idet, const int ihar, const int imul)
{
 if(idet<NDET) {
  if(idet<0 || idet>NDET-1 || ihar<0 || ihar>NHAR-1 || imul<0 || imul>NMUL-1) {
    if(verbosity>1){
      cout << PHWHERE << " Error : check index (id,ih,im)=("
           << idet << "," << ihar << "," << imul << ")" << endl;
    }
    return 0.0;
  }
  return SumYmean[idet][ihar][imul];
 }else{
  if(idet<0 || idet>NDET2-1 || ihar<0 || ihar>NHAR2-1 || imul<0 || imul>NMUL2-1) {
    if(verbosity>1){
      cout << PHWHERE << " Error : check index2 (id,ih,im)=("
           << idet << "," << ihar << "," << imul << ")" << endl;
    }
    return 0.0;
  }
  return SumYmean2[idet-NDET][ihar][imul];
 }
}

float ReactionPlaneCalib::GetSumYsigma(const int idet, const int ihar, const int imul)
{
 if(idet<NDET) {
  if(idet<0 || idet>NDET-1 || ihar<0 || ihar>NHAR-1 || imul<0 || imul>NMUL-1) {
    if(verbosity>1){
      cout << PHWHERE << " Error : check index (id,ih,im)=("
           << idet << "," << ihar << "," << imul << ")" << endl;
    }
    return 1.0;
  }
  return SumYsigma[idet][ihar][imul];
 }else{
  if(idet<0 || idet>NDET2-1 || ihar<0 || ihar>NHAR2-1 || imul<0 || imul>NMUL2-1) {
    if(verbosity>1){
      cout << PHWHERE << " Error : check index (id,ih,im)=("
           << idet << "," << ihar << "," << imul << ")" << endl;
    }
    return 1.0;
  }
  return SumYsigma2[idet-NDET][ihar][imul];
 }
}

float ReactionPlaneCalib::GetFlatCos(const int idet, const int ihar, const int imul,
                                     const int izps, const int iord)
{
 if(idet<NDET) {
  if(idet<0 || idet>NDET-1 || ihar<0 || ihar>NHAR-1 || imul<0 || imul>NMUL-1 || izps<0 || izps>NZPS-1 || iord<0 || iord>NORD-1) {
    if(verbosity>1){
      cout << PHWHERE << " Error : check index (id,ih,im,iz,io)=("
           << idet << "," << ihar << "," << imul << "," << izps << "," << iord << ")" << endl;
    }
    return 0.0;
  }
  return FlatCos[idet][ihar][imul][izps][iord];
 }else{
  if(idet<0 || idet>NDET2-1 || ihar<0 || ihar>NHAR2-1 || imul<0 || imul>NMUL2-1 || izps<0 || izps>NZPS2-1 || iord<0 || iord>NORD2-1) {
    if(verbosity>1){
      cout << PHWHERE << " Error : check index (id,ih,im,iz,io)=("
           << idet << "," << ihar << "," << imul << "," << izps << "," << iord << ")" << endl;
    }
    return 0.0;
  }
  return FlatCos2[idet-NDET][ihar][imul][izps][iord];
 }
}

float ReactionPlaneCalib::GetFlatSin(const int idet, const int ihar, const int imul,
                                     const int izps, const int iord)
{
 if(idet<NDET) {
  if(idet<0 || idet>NDET-1 || ihar<0 || ihar>NHAR-1 || imul<0 || imul>NMUL-1 || izps<0 || izps>NZPS-1 || iord<0 || iord>NORD-1) {
    if(verbosity>1){
      cout << PHWHERE << " Error : check index (id,ih,im,iz,io)=("
           << idet << "," << ihar << "," << imul << "," << izps << "," << iord << ")" << endl;
    }
    return 0.0;
  }
  return FlatSin[idet][ihar][imul][izps][iord];
 }else{
  if(idet<0 || idet>NDET2-1 || ihar<0 || ihar>NHAR2-1 || imul<0 || imul>NMUL2-1 || izps<0 || izps>NZPS2-1 || iord<0 || iord>NORD2-1) {
    if(verbosity>1){
      cout << PHWHERE << " Error : check index (id,ih,im,iz,io)=("
           << idet << "," << ihar << "," << imul << "," << izps << "," << iord << ")" << endl;
    }
    return 0.0;
  }
  return FlatSin2[idet-NDET][ihar][imul][izps][iord];
 }
}

int ReactionPlaneCalib::GetCentrality(PHCompositeNode *topNode)
{
  RunHeader *run = findNode::getClass<RunHeader>(topNode,"RunHeader");
  PHGlobal *global = findNode::getClass<PHGlobal>(topNode,"PHGlobal");
  int runNumber = run->get_RunNumber();

  // Run4
  if( runNumber < (int)BEGIN_OF_RUN4 ){
    cout << PHWHERE << " ReactionPlaneCalib is implemented from Run4. run = " << runNumber << endl;
    return -1;
  }
  else if( runNumber < 122337 ) { // end of Run4 Au+Au 200 GeV
    return PhUtilities::getCentralityByClockRun4(topNode);
  }
  else if( runNumber < 123565 ) {// end of Run4 Au+Au 62.4 GeV
    // Need to register 
    //  - BunchCross
    //  - PercentileRecalReco 
    //  in your analysis macro

    return (int)global->getCentrality();
  }

  // Run5 200 GeV or 62.4 GeV
  // 200 GeV  ( Run 150292 - 160487 )
  // 62.4 GeV ( Run 161196 - 163463 )
  if( runNumber > (int)BEGIN_OF_RUN5 && runNumber < 163464 ) {
    // Need to register
    // - BunchCross
    // - Run5CuCu200GeVCentralityReco
    // or Run5CuCu62GeVCentralityReco

    return (int)global->getCentrality();
  }

  return -1;
}

int ReactionPlaneCalib::GetCentralityBin(const int cent)
{
  int icent = (int) ( NMUL * ((cent - 0.001) / 100.) );

  if(icent<0 || icent>NMUL-1) icent = -1;

  return icent;
}
int ReactionPlaneCalib::GetCentralityBin2(const int cent)
{
  int icent = (int) ( NMUL2 * ((cent - 0.001) / 100.) );
  if(icent<0 || icent>NMUL2-1) icent = -1;
  return icent;
}

int ReactionPlaneCalib::GetZVertexBin(const float bbcz)
{
  int ibbcz = (int) ( NZPS * ((bbcz + 30.0) / 60.0));

  if(ibbcz<0 || ibbcz>NZPS-1) ibbcz = -1;

  return ibbcz;
}
int ReactionPlaneCalib::GetZVertexBin2(const float bbcz)
{
  int ibbcz = (int) ( NZPS2 * ((bbcz + 30.0) / 60.0));
  if(ibbcz<0 || ibbcz>NZPS2-1) ibbcz = -1;
  return ibbcz;
}


int ReactionPlaneCalib::GetBankID() const
{
  // return bank id 

  if( RunNumber > BEGIN_OF_RUN7 ){
    return 2;
  }
  else if( RunNumber > BEGIN_OF_RUN4 && RunNumber < BEGIN_OF_RUN7 ){
    return 1;
  }
  else{
    cout << PHWHERE << " Invalid run number for reaction plane, run = " << RunNumber << endl;
    cout << " return default value (1)" << endl;
    return 1;
  }
  

}

int ReactionPlaneCalib::GetDBLength(const int bankid) const
{
  // return DB length

  int length_sum = NDET * NHAR * NMUL * 4;
  int length_flat = NDET * NHAR * NMUL * NZPS * NORD;

  // Add RXN/MPC from Run7
  if( bankid == 2 ){
    cout << "Add RXN and MPC stuffs ..." << endl;
    length_sum  += (NDET2-NDET) * NHAR2 * NMUL2 * 4 ;
    length_flat += (NDET2-NDET) * NHAR2 * NMUL2 * NZPS2 * NORD2;
  }

  return length_sum + 2 * length_flat + 2;
}


