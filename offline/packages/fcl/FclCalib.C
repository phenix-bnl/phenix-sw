#include "FclIndexer.h"
#include "FclCalib.h"

#include "PdbApplication.hh"
#include "PdbCalBank.hh"
#include "PdbFclGain.hh"
#include "PdbBankManager.hh"

#include "PHTimeStamp.h"

#include <iostream>

using namespace std;

FclCalib::FclCalib()
{
  whichSide = -1;
  for (int i = 0;i < ROWUSE;i++)
    {
      for (int j = 0;j < COLUSE;j++)
        {
          zdcxtalk_intercept[i][j] = -1.0;
          zdcxtalk_intercept_err[i][j] = -1.0;
          zdcxtalk_slope[i][j] = -1.0;
          zdcxtalk_slope_err[i][j] = -1.0;
        }
    }
}

float FclCalib::getCalib(int channel)
{
  FclIndexer *indexer = FclIndexer::Instance();
  if (indexer->getRow(whichSide, channel) > -1 &&
      indexer->getRow(whichSide, channel) < ROWUSE &&
      indexer->getColumn(whichSide, channel) > -1 &&
      indexer->getColumn(whichSide, channel) < COLUSE)
    {
      return cal[indexer->getRow(whichSide, channel)][indexer->getColumn(whichSide, channel)];
    }
  else
    {
      return -2;
    }
}

int FclCalib::getDatabaseInfo(PHTimeStamp &time)
{
  PdbBankManager *bankManager = PdbBankManager::instance();
  PdbApplication *application = bankManager->getApplication();
  if (application->startRead())
    {
      PHTimeStamp tSearch;
      PdbBankID bankID(0);

      PdbCalBank *fclBank;
      PdbCalBank *fclXtalkBank;
      tSearch = time;

      bankID.setInternalValue(1);
      fclXtalkBank = bankManager->fetchBank("PdbFclGainBank", bankID, "calib.fcl.zdcxtalk", tSearch);

      bankID.setInternalValue(0);
      fclBank = bankManager->fetchBank("PdbFclGainBank", bankID, "calib.fcl.cosmic", tSearch);

      if (fclBank && fclXtalkBank)
        {
          cout << "FCAL calibration search for time:" << tSearch << " actual database time:" << fclBank->getStartValTime() << endl;
          PdbFclGain* fcl = (PdbFclGain*) & (fclBank->getEntry(0));
          PdbFclGain* fcl_intercepts = (PdbFclGain*) & (fclXtalkBank->getEntry(0));
          PdbFclGain* fcl_slopes = (PdbFclGain*) & (fclXtalkBank->getEntry(1));

          if (!(fcl && fcl_intercepts && fcl_slopes))
            {
              cout << "FCL calibration entries not found in bank" << endl;
              delete fclBank;
              delete fclXtalkBank;
              return 0;

            }
          else
            {
              fcl_intercepts->print();
              fcl_slopes->print();
              fcl->print();
            }

          int calerr = 0;
          for (int row = 0; row < ROWUSE; row++)

            {
              for (int col = 0; col < COLUSE; col++)
                {
                  if (whichSide == FCALNORTH)
                    {
                      if (fcl->getNorthGain(row, col) <= 0)
                        {
                          //			  cout << "ERROR:: FCAL calibration error " << PHWHERE << endl;
                          if (calerr == 0)
                            cout << "Warning:: FCAL North calibration constant is negative for (row,column) =";
                          cout << " (" << row << "," << col << ")";
                          calerr++;
                          cal[row][col] = -1;
                        }
                      else
                        {
                          cal[row][col] = fcl->getNorthGain(row, col);
                        }
                      zdcxtalk_intercept[row][col] = fcl_intercepts->getNorthGain(row, col);
                      zdcxtalk_intercept_err[row][col] = fcl_intercepts->getNorthGainError(row, col);
                      zdcxtalk_slope[row][col] = fcl_slopes->getNorthGain(row, col);
                      zdcxtalk_slope_err[row][col] = fcl_slopes->getNorthGainError(row, col);
                    }
                  else if (whichSide == FCALSOUTH)
                    {
                      if (fcl->getSouthGain(row, col) <= 0)
                        {
                          //			  cout << "ERROR:: FCAL calibration error " << PHWHERE << endl;
                          if (calerr == 0)
                            cout << "Warning:: FCAL South calibration constant is negative for (row,column) =";
                          cout << " (" << row << "," << col << ")";
                          calerr++;
                          cal[row][col] = -1;
                        }
                      else
                        {
                          cal[row][col] = fcl->getSouthGain(row, col);
                        }
                      zdcxtalk_intercept[row][col] = fcl_intercepts->getSouthGain(row, col);
                      zdcxtalk_intercept_err[row][col] = fcl_intercepts->getSouthGainError(row, col);
                      zdcxtalk_slope[row][col] = fcl_slopes->getSouthGain(row, col);
                      zdcxtalk_slope_err[row][col] = fcl_slopes->getSouthGainError(row, col);
                    }
                  else
                    {
                      delete fclBank;
                      delete fclXtalkBank;
                      return 0;
                    }
                }
            }
          if (calerr > 0)
	    {
              cout << endl;
	    }
          delete fclBank;
          delete fclXtalkBank;
        }
      else
        {
	  if (fclBank)
	    {
	      delete fclBank;
	    }
	  if (fclXtalkBank)
	    {
	      delete fclXtalkBank;
	    }
          cout << "FCL calibration bank not found" << endl;
          return 0;
        }
    }
  return 1;
}

int FclCalib::getDatabaseInfo(int runNumber)
{
  int runHigh;
  int runLow;
  const char *calibname = "calib.fcl.mon";
  PdbBankManager *bankManager = PdbBankManager::instance();
  PdbApplication *application = bankManager->getApplication();
  if (application->startRead())
    {

      PHTimeStamp tSearch;
      PdbBankID bankID(0);

      //get the "static" runs that are used in the normalization

      tSearch.setTics(FCALHIGHLOWRUN);
      PdbCalBank *fclBank;
      fclBank = bankManager->fetchBank("PdbFclGainBank", bankID, "calib.fcl.lowhigh", tSearch);
      PdbFclGain* fcl_lowhigh = NULL;
      if (fclBank)
        {
          fcl_lowhigh = (PdbFclGain*) & (fclBank->getEntry(0));
        }
      else
        {
          cout << "ERROR: can't read calibration database" << PHWHERE << endl;
        }

      tSearch.setTics(FCALCOSMICRUN);
      fclBank = bankManager->fetchBank("PdbFclGainBank", bankID, "calib.fcl.cosmic", tSearch);
      PdbFclGain* fcl_cosmic = NULL;
      if (fclBank)
        {
          fcl_cosmic = (PdbFclGain*) & (fclBank->getEntry(0));
        }
      else
        {
          cout << "ERROR: can't read calibration database" << PHWHERE << endl;
        }

      tSearch.setTics(FCALBASERUN);
      fclBank = bankManager->fetchBank("PdbFclGainBank", bankID, calibname, tSearch);
      PdbFclGain* fcl_base = NULL;
      if (fclBank)
        {
          fcl_base = (PdbFclGain*) & (fclBank->getEntry(0));
        }
      else
        {
          cout << "ERROR: can't read calibration database" << PHWHERE << endl;
        }

      PdbFclGain* fcl_static = new PdbFclGain();
      for (int row = 0; row < ROWUSE; row++)
        {
          for (int col = 0; col < COLUSE; col++)
            {
              float tmp;
              tmp = fcl_lowhigh->getNorthGain(row, col);
              tmp = tmp * fcl_cosmic->getNorthGain(row, col);
              tmp = tmp / fcl_base->getNorthGain(row, col);
              fcl_static->setNorthGain(row, col, tmp , 0.0);

              tmp = fcl_lowhigh->getSouthGain(row, col);
              tmp = tmp * fcl_cosmic->getSouthGain(row, col);
              tmp = tmp / fcl_base->getSouthGain(row, col);
              fcl_static->setSouthGain(row, col, tmp , 0.0);
            }
        }
      //get the bank for the current run or interpolate.

      tSearch.setTics(runNumber);

      PdbCalBank *fclBankHigh;
      PdbCalBank *fclBankLow;
      fclBank = bankManager->fetchBank("PdbFclGainBank", bankID, calibname, tSearch);
      fclBankHigh = bankManager->fetchBank("PdbFclGainBank", bankID, calibname, tSearch);
      fclBankLow = bankManager->fetchBank("PdbFclGainBank", bankID, calibname, tSearch);


      if (fclBank)
        {
          cout << "FCAL calibration search for time:" << tSearch << " actual database time:" << fclBank->getStartValTime() << endl;
          PdbFclGain* fcl = (PdbFclGain*) & (fclBank->getEntry(0));
          for (int row = 0; row < ROWUSE; row++)
            {
              for (int col = 0; col < COLUSE; col++)
                {
                  if (whichSide == FCALNORTH)
                    {
                      if (fcl->getNorthGain(row, col) <= 0)
                        {
                          cout << "ERROR:: FCAL calibration error " << PHWHERE << endl;
                          cal[row][col] = -1;
                        }
                      else
                        {
                          cal[row][col] = fcl->getNorthGain(row, col) *
			    fcl_static->getNorthGain(row, col);
                        }
                    }
                  else if (whichSide == FCALSOUTH)
                    {
                      if (fcl->getSouthGain(row, col) <= 0)
                        {
                          cout << "ERROR:: FCAL calibration error " << PHWHERE << endl;
                          cal[row][col] = -1;
                        }
                      else
                        {
                          cal[row][col] = fcl->getSouthGain(row, col) *
			    fcl_static->getSouthGain(row, col);
                        }
                    }
                  else
                    {
                      return 0;
                    }
                }
            }
        }
      else
        {
          cout << "FCL calibration bank not found interpolating" << endl;
          runLow = runNumber;
          while (runLow > 69500 && !fclBankLow)
            {
              runLow--;
              tSearch.setTics(runLow);
              fclBankLow = bankManager->fetchBank("PdbFclGainBank", bankID, calibname, tSearch);
            }
          cout << "Low run number:" << runLow << endl;
          runHigh = runNumber;
          while (runHigh < 82000 && !fclBankHigh)
            {
              runHigh++;
              tSearch.setTics(runHigh);
              fclBankHigh = bankManager->fetchBank("PdbFclGainBank", bankID, calibname, tSearch);
            }
          cout << "High run number:" << runHigh << endl;

          if (fclBankHigh && fclBankLow)
            {

              cout << "FCAL calibration search for time not found " << " actual database times:" << fclBankLow->getStartValTime() << " and " << fclBankHigh->getStartValTime() << endl;
              PdbFclGain* fclHigh = (PdbFclGain*) & (fclBankHigh->getEntry(0));
              PdbFclGain* fclLow = (PdbFclGain*) & (fclBankLow->getEntry(0));
              for (int row = 0; row < ROWUSE; row++)
                {
                  for (int col = 0; col < COLUSE; col++)
                    {
                      if (whichSide == FCALNORTH)
                        {
                          if (fclHigh->getNorthGain(row, col) <= 0 ||
                              fclLow->getNorthGain(row, col) <= 0)
                            {
                              cout << "ERROR:: FCAL calibration error " << PHWHERE << endl;
                              cal[row][col] = -1;
                            }
                          else
                            {
                              float tmp;
                              tmp = (fclLow->getNorthGain(row, col) -
                                     fclHigh->getNorthGain(row, col) ) /
				(runLow - runHigh);
                              tmp = tmp * (runLow - runNumber);
                              cal[row][col] = fclLow->getNorthGain(row, col) - tmp;
                              cal[row][col] = cal[row][col] * fcl_static->getNorthGain(row, col);
                            }
                        }
                      else if (whichSide == FCALSOUTH)
                        {
                          if (fclLow->getSouthGain(row, col) <= 0 ||
                              fclHigh->getSouthGain(row, col) <= 0)
                            {
                              cout << "ERROR:: FCAL calibration error " << PHWHERE << endl;
                              cal[row][col] = -1;
                            }
                          else
                            {
                              float tmp;
                              tmp = (fclLow->getSouthGain(row, col) -
                                     fclHigh->getSouthGain(row, col) ) /
				(runLow - runHigh);
                              tmp = tmp * (runLow - runNumber);
                              cal[row][col] = fclLow->getSouthGain(row, col) - tmp;
                              cal[row][col] = cal[row][col] * fcl_static->getSouthGain(row, col);
                            }
                        }
                    }
                }
            }
          else
            {
              cout << "ERROR in FCAL calibration " << PHWHERE << endl;
              return 0;
            }
        }
      delete fcl_static;
    }
  return 1;
}


int FclCalib::getDatabaseInfoCosmicOnly()
{
  cout << "Caution, you are only reading the cosmic data in the calibration" << endl;
  PdbBankManager *bankManager = PdbBankManager::instance();
  PdbApplication *application = bankManager->getApplication();
  if (application->startRead())
    {

      PHTimeStamp tSearch;
      PdbBankID bankID(0);

      //get the "static" runs that are used in the normalization

      tSearch.setTics(FCALHIGHLOWRUN);
      PdbCalBank *fclBank;
      fclBank = bankManager->fetchBank("PdbFclGainBank", bankID, "calib.fcl.lowhigh", tSearch);
      PdbFclGain* fcl_lowhigh = NULL;
      if (fclBank)
        {
          fcl_lowhigh = (PdbFclGain*) & (fclBank->getEntry(0));
        }
      else
        {
          cout << "ERROR: can't read calibration database" << PHWHERE << endl;
        }

      tSearch.setTics(FCALCOSMICRUN);
      fclBank = bankManager->fetchBank("PdbFclGainBank", bankID, "calib.fcl.cosmic", tSearch);
      PdbFclGain* fcl_cosmic = NULL;
      if (fclBank)
        {
          fcl_cosmic = (PdbFclGain*) & (fclBank->getEntry(0));
        }
      else
        {
          cout << "ERROR: can't read calibration database" << PHWHERE << endl;
        }

      PdbFclGain* fcl_static = new PdbFclGain();
      for (int row = 0; row < ROWUSE; row++)
        {
          for (int col = 0; col < COLUSE; col++)
            {
              float tmp;
              tmp = fcl_lowhigh->getNorthGain(row, col);
              tmp = tmp * fcl_cosmic->getNorthGain(row, col);
              fcl_static->setNorthGain(row, col, tmp , 0.0);

              tmp = fcl_lowhigh->getSouthGain(row, col);
              tmp = tmp * fcl_cosmic->getSouthGain(row, col);
              fcl_static->setSouthGain(row, col, tmp , 0.0);
            }
        }

      if (fclBank)
        {
          for (int row = 0; row < ROWUSE; row++)
            {
              for (int col = 0; col < COLUSE; col++)
                {
                  if (whichSide == FCALNORTH)
                    {
                      cal[row][col] = fcl_static->getNorthGain(row, col);

                    }
                  else if (whichSide == FCALSOUTH)
                    {
                      cal[row][col] = fcl_static->getSouthGain(row, col);
                    }
                  else
                    {
                      return 0;
                    }
                }
            }
        }
      delete fcl_static;
    }
  return 1;
}


void FclCalib::print()
{
  cout << endl << endl << "Contents of Calib object" << endl;
  cout << "Side :" << whichSide << endl;
  for (int row = 0; row < ROWUSE; row++)
    {
      for (int col = 0; col < COLUSE; col++)
        {
          cout << cal[row][col] << " ";
        }
      cout << endl;
    }
}

void
FclCalib::getZdcXtalk(int row, int col, float &intercept, float &slope, float &intercept_err, float &slope_err)
{
  if (0 <= row && row < ROWUSE && 0 <= col && col < COLUSE)
    {
      intercept = zdcxtalk_intercept[row][col];
      intercept_err = zdcxtalk_intercept_err[row][col];
      slope = zdcxtalk_slope[row][col];
      slope_err = zdcxtalk_slope_err[row][col];
    }
  else
    {
      cout << PHWHERE << " Row Column out of range!\n";
      intercept = -1.0;
      intercept_err = -1.0;
      slope = -1.0;
      slope_err = -1.0;
    }
  return ;
}



