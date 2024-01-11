//-------------------------------------------------------------------
// Implementation of class TofwCalib
// Author : Shengli Huang, Vanderbilt Univ.   
//          (shengli.huang@vanderbilt.edu)
//
//-------------------------------------------------------------------

#include <TofwCalib.h>

#include <PdbBankManager.hh>
#include <PdbApplication.hh>
#include <PdbCalBank.hh>
#include <PdbParameter.hh>

#include <RunToTime.hh>


#include <iostream>
#include <fstream>

using namespace std;

TofwCalib::TofwCalib()
{
  debug=0;

  nruncalib=0;

  for(int ich=0; ich< TOFW_NCHAMBER_BOX*TOFW_NSTRIP_CHAMBER; ich++){
    TVC_ConvSouthBottom[ich] = -9999;
    TVC_ConvNorthBottom[ich] = -9999; 
    TVC_ConvSouthTop[ich] = -9999;
    TVC_ConvNorthTop[ich] = -9999;
  }

  for(int istrip=0; istrip<TOFW_NSTRIP_TOTAL; istrip++){
    toffset[istrip]=0;
  }
}

int TofwCalib::get_end(int ich)
{
  return ich%8/4;
}


int TofwCalib::get_strip(int ich)
{
  return ich%8%4;
}

int TofwCalib::get_chamber(int iboard, int ich)
{
  int ichamber = -9999;
  if(ich<8) ichamber = 2*(15-iboard);
  else ichamber = 2*(15-iboard)+1;
  return ichamber;
}

float TofwCalib::getTvcConv(int ibox, int ichamber, int istrip, int iend)
{
  int iboard = 15 - ichamber/2;
  int ichannel = iboard*16 + ichamber%2*8 + 4*iend +istrip;
  if(ibox==0)      return TVC_ConvSouthBottom[ichannel];
  else if(ibox==1) return TVC_ConvNorthBottom[ichannel];
  else if(ibox==2) return TVC_ConvSouthTop[ichannel];
  else if(ibox==3) return TVC_ConvNorthTop[ichannel];
  else return -9999;
}

float TofwCalib::getTvcConv(int ibox, int ichannel)
{
  if(ibox==0)      return TVC_ConvSouthBottom[ichannel];
  else if(ibox==1) return TVC_ConvNorthBottom[ichannel];
  else if(ibox==2) return TVC_ConvSouthTop[ichannel];
  else if(ibox==3) return TVC_ConvNorthTop[ichannel];
  else return -9999;
}

bool TofwCalib::getGoodStrip(int istrip)
{
  if(toffset[istrip]<-9990) return false;
  else return true;
}

float TofwCalib::get_toffset(int istrip)
{
  return toffset[istrip];
}

bool TofwCalib::set_toffset(int istrip, float value)
{
  toffset[istrip]=value;
  return true;
}

int TofwCalib::Channel2Strip(int ch)
{
  return ch2strip[ch][1];
}

int TofwCalib::Channel2End(int ch)
{
  return ch2strip[ch][2];
}

bool TofwCalib::setTvcConv(int ibox, int ich, float value){
  if(ibox==0) 
    { 
      TVC_ConvSouthBottom[ich]=value;
      return 1;
    }
  else if(ibox==1)
    {
      TVC_ConvNorthBottom[ich]=value;
      return 1;
    }
  else if(ibox==2) {
    TVC_ConvSouthTop[ich]=value;
    return 1;
  }
  else if(ibox==3) {
    TVC_ConvNorthTop[ich]=value;
    return 1;
  }
  else return 0;
}

PHBoolean TofwCalib::fetchTvcConv(const int run) 
{
  PdbBankManager *bankManager = PdbBankManager::instance();
  PdbApplication *application = bankManager->getApplication();
  
  if (!application->startRead())
    {
      PHMessage("TofwCalib::fetchTvcConv", PHError, "Aborting... Database not readable");
      application->abort();
    }
  
  //  Make a bank ID(bankName)
  PdbBankID bankID;
  bankID.setInternalValue(1);
  const char* dataName = "calib.tofw.tdc";

  //  Grap a pointer to the bank with name dataName
  PdbParameter *parameter;
  int index = 0;
  PdbCalBank *tofwBank = bankManager->fetchBank("PdbParameterBank", bankID, dataName, run);
  
  int length = TOFW_NCH_TOTAL + 2;
  int truelength = tofwBank->getLength();
  
  if (length != truelength)
    {
      cout << PHWHERE;
      cout << "TofwCalib::fetchTvcConv FATAL...wrong length DB read" 
	   << truelength << endl;
      cout << "expected length: " << length << endl;
      return 0;
    }
  
  // Recognized scheme check
  parameter = (PdbParameter *) & tofwBank->getEntry(index++);
  int scheme = (int)parameter->getParameter();
  if (scheme != 1)
    {
      cout << "TofwCalib::fetchTvcConv FATAL...unknown scheme DB read:" 
	   << scheme << endl;
      return 0;
    }
  
  // Internal data entries check
  parameter = (PdbParameter *) & tofwBank->getEntry(index++);
  int entries = (int)parameter->getParameter();
  if (entries != length - 2)
    {
      cout << "TofwCalib::fetchTvcConv FATAL...wrong entries DB read" << endl;
      return 0;
    }

  // Get the parameters...
  if(debug)
    {
      cout << "TofwCalib::fetchTvcConv READING from Database..." << endl;
      tofwBank->print();
      tofwBank->printHeader();
      cout << "tofwBank length (TvcConv) = " << tofwBank->getLength() << endl;
    }

  for (int i = 0; i < TOFW_NCH_TOTAL; i++)
    {
      parameter = (PdbParameter *) & tofwBank->getEntry(index++);
      float tvcconv  = parameter->getParameter();
      int ibox = int(i/256);
      int ich  = int(i%256);
      setTvcConv(ibox, ich, tvcconv);
      if(debug>0) cout<<ich<<" "<<tvcconv<<endl;
    }
  
  delete tofwBank;
  return True;
}

PHBoolean TofwCalib::fetchToffset(const int run) 
{
  PdbBankManager *bankManager = PdbBankManager::instance();
  PdbApplication *application = bankManager->getApplication();
  
  if (!application->startRead())
    {
      PHMessage("TofwCalib::fetchToffset", PHError, "Aborting... Database not readable");
      application->abort();
    }
  
  //  Make a bank ID(bankName)
  PdbBankID bankID;
  bankID.setInternalValue(1);
  const char* dataName = "calib.tofw.Toffset";

  //  Grap a pointer to the bank with name dataName
  PdbParameter *parameter;
  int index = 0;
  PdbCalBank *tofwBank = bankManager->fetchBank("PdbParameterBank", bankID, dataName, run);
  
  int length = TOFW_NSTRIP_TOTAL + 2;
  int truelength = tofwBank->getLength();
  
  if (length != truelength)
    {
      cout << PHWHERE;
      cout << "TofwCalib::fetchToffset FATAL...wrong length DB read" 
	   << truelength << endl;
      cout << "expected length: " << length << endl;
      return 0;
    }
  
  // Recognized scheme check
  parameter = (PdbParameter *) & tofwBank->getEntry(index++);
  int scheme = (int)parameter->getParameter();
  if (scheme != 1)
    {
      cout << "TofwCalib::fetchToffset FATAL...unknown scheme DB read:" 
	   << scheme << endl;
      return 0;
    }
  
  // Internal data entries check
  parameter = (PdbParameter *) & tofwBank->getEntry(index++);
  int entries = (int)parameter->getParameter();
  if (entries != length - 2)
    {
      cout << "TofwCalib::fetchToffset FATAL...wrong entries DB read" << endl;
      return 0;
    }

  // Get the parameters...
  if(debug)
    {
      cout << "TofwCalib::fetchToffset READING from Database..." << endl;
      tofwBank->print();
      tofwBank->printHeader();
      cout << "tofwBank length (Toffset) = " << tofwBank->getLength() << endl;
    }

  for (int i = 0; i < TOFW_NSTRIP_TOTAL; i++)
    {
      parameter = (PdbParameter *) & tofwBank->getEntry(index++);
      float t_offset  = parameter->getParameter();
      set_toffset(i, t_offset);
      if(debug>0) cout<<i<<" "<<t_offset<<endl;
    }
  
  delete tofwBank;
  return True;
}

PHBoolean TofwCalib::fetchstripoff(const int run) 
{
  PdbBankManager *bankManager = PdbBankManager::instance();
  PdbApplication *application = bankManager->getApplication();
  
  if (!application->startRead())
    {
      PHMessage("TofwCalib::fetchstripoff", PHError, "Aborting... Database not readable");
      application->abort();
    }
  
  //  Make a bank ID(bankName)
  PdbBankID bankID;
  bankID.setInternalValue(1);
  const char* dataName = "calib.tofw.stripoff";

  //  Grap a pointer to the bank with name dataName
  PdbParameter *parameter;
  int index = 0;
  PdbCalBank *tofwBank = bankManager->fetchBank("PdbParameterBank", bankID, dataName, run);
  
  int length = TOFW_NSTRIP_TOTAL*7 + 8 + 2;//8 matching parameter
  int truelength = tofwBank->getLength();
  
  if (length != truelength)
    {
      cout << PHWHERE;
      cout << "TofwCalib::fetchstripoff FATAL...wrong length DB read" 
	   << truelength << endl;
      cout << "expected length: " << length << endl;
      return 0;
    }
  
  // Recognized scheme check
  parameter = (PdbParameter *) & tofwBank->getEntry(index++);
  int scheme = (int)parameter->getParameter();
  if (scheme != 1)
    {
      cout << "TofwCalib::fetchstripoff FATAL...unknown scheme DB read:" 
	   << scheme << endl;
      return 0;
    }
  
  // Internal data entries check
  parameter = (PdbParameter *) & tofwBank->getEntry(index++);
  int entries = (int)parameter->getParameter();
  if (entries != length - 2)
    {
      cout << "TofwCalib::fetchstripoff FATAL...wrong entries DB read" << endl;
      return 0;
    }

  if(debug)
    {
      // Get the parameters...
      cout << "TofwCalib::fetchstripoff READING from Database..." << endl;
      tofwBank->print();
      tofwBank->printHeader();
      cout << "tofwBank length (stripoff) = " << tofwBank->getLength() << endl;
    }

  for(int i=0; i<4; i++){
    parameter = (PdbParameter *) & tofwBank->getEntry(index++);
    float sigma_dphi  = parameter->getParameter();
    set_sigma_dphi(i,sigma_dphi);

    parameter = (PdbParameter *) & tofwBank->getEntry(index++);
    float sigma_dz  = parameter->getParameter();
    set_sigma_dz(i,sigma_dz);
  }

  for (int i = 0; i < TOFW_NSTRIP_TOTAL; i++)
    {
      parameter = (PdbParameter *) & tofwBank->getEntry(index++);
      float deltaT  = parameter->getParameter();
      set_DeltaT(i, deltaT);
      if(debug>0) cout<<i<<" "<<DeltaT[i]<<" ";
      
      parameter = (PdbParameter *) & tofwBank->getEntry(index++);
      float slewing_a  = parameter->getParameter();
      set_Slewing_A(i, slewing_a);
      if(debug>0) cout<<Slewing_A[i]<<" ";

      parameter = (PdbParameter *) & tofwBank->getEntry(index++);
      float slewing_b  = parameter->getParameter();
      set_Slewing_B(i, slewing_b);
      if(debug>0) cout<<Slewing_B[i]<<" ";

      parameter = (PdbParameter *) & tofwBank->getEntry(index++);
      float mean_dz_plus  = parameter->getParameter();
      set_Mean_Dz_Plus(i, mean_dz_plus);
      if(debug>0) cout<<Mean_Dz_Plus[i]<<" ";

      parameter = (PdbParameter *) & tofwBank->getEntry(index++);
      float mean_dz_minus  = parameter->getParameter();
      set_Mean_Dz_Minus(i, mean_dz_minus);
      if(debug>0) cout<<Mean_Dz_Minus[i]<<" ";

      parameter = (PdbParameter *) & tofwBank->getEntry(index++);
      float mean_dphi_plus  = parameter->getParameter();
      set_Mean_Dphi_Plus(i, mean_dphi_plus);
      if(debug>0) cout<<Mean_Dphi_Plus[i]<<" ";

      parameter = (PdbParameter *) & tofwBank->getEntry(index++);
      float mean_dphi_minus  = parameter->getParameter();
      set_Mean_Dphi_Minus(i, mean_dphi_minus);
      if(debug>0) cout<<Mean_Dphi_Minus[i]<<endl;
    }
  
  delete tofwBank;
  return True;
}

PHBoolean TofwCalib::fetchrunoff(const int run) 
{
  PdbBankManager *bankManager = PdbBankManager::instance();
  PdbApplication *application = bankManager->getApplication();
  
  if (!application->startRead())
    {
      PHMessage("TofwCalib::fetchrunoff", PHError, "Aborting... Database not readable");
      application->abort();
    }
  
  //  Make a bank ID(bankName)
  PdbBankID bankID;
  bankID.setInternalValue(1);
  const char* dataName = "calib.tofw.runoff";

  //  Grap a pointer to the bank with name dataName
  PdbParameter *parameter;
  int index = 0;
  PdbCalBank *tofwBank = bankManager->fetchBank("PdbParameterBank", bankID, dataName, run);
  
  int length = 2 + 2;
  int truelength = tofwBank->getLength();
  
  if (length != truelength)
    {
      cout << PHWHERE;
      cout << "TofwCalib::fetchrunoff FATAL...wrong length DB read" 
	   << truelength << endl;
      cout << "expected length: " << length << endl;
      return 0;
    }
  
  // Recognized scheme check
  parameter = (PdbParameter *) & tofwBank->getEntry(index++);
  int scheme = (int)parameter->getParameter();
  if (scheme != 1)
    {
      cout << "TofwCalib::fetchrunoff FATAL...unknown scheme DB read:" 
	   << scheme << endl;
      return 0;
    }
  
  // Internal data entries check
  parameter = (PdbParameter *) & tofwBank->getEntry(index++);
  int entries = (int)parameter->getParameter();
  if (entries != length - 2)
    {
      cout << "TofwCalib::fetchrunoff FATAL...wrong entries DB read" << endl;
      return 0;
    }

  // Get the parameters...
  if(debug)
    {
      cout << "TofwCalib:: fetchrunoff "<<run<< " READING from Database..." << endl;
      tofwBank->print();
      tofwBank->printHeader();
      cout << "tofwBank length (runoff) = " << tofwBank->getLength() << endl;
    }

  
  parameter = (PdbParameter *) & tofwBank->getEntry(index++);
  float runnumber  = parameter->getParameter();
  
  parameter = (PdbParameter *) & tofwBank->getEntry(index++);
  float runoffset  = parameter->getParameter();
  set_runoffset(runoffset);
  if(debug>0) cout<<runnumber<<" "<<runoffset<<endl;
  
  delete tofwBank;
  return True;
}

PHBoolean TofwCalib::fetchTvcConv(const int ibox, const char *filename)
{
  ifstream fin(filename);
  if(!fin)
    {
      cout << PHWHERE << " can't open " << filename << endl;
      return False;
    }
  
  for(int ich=0; ich<TOFW_NBOARD * TOFW_NCH; ich++)
    {
      float tvcconv;
      fin >> tvcconv;

      setTvcConv(ibox, ich, tvcconv);
    }
  
  cout << endl << "Fetched TvcConv from " << filename 
       << " [ASCII file]" << endl;
  fin.close();
  
  return True;
}

PHBoolean TofwCalib::fetchToffset(const char *filename)
{
  ifstream fin(filename);
  if(!fin)
    {
      cout << PHWHERE << " can't open " << filename << endl;
      return False;
    }
  
  for(int ich=0; ich<TOFW_NSTRIP_TOTAL; ich++)
    {
      float t_offset;
      fin >> t_offset;

      set_toffset(ich, t_offset);
    }
  
  cout << endl << "Fetched Toffset from " << filename 
       << " [ASCII file] " <<  endl;
  fin.close();
  
  return True;
}

PHBoolean TofwCalib::fetchstripoff(const char *filename)
{
  ifstream fin(filename);
  if(!fin)
    {
      cout << PHWHERE << " can't open " << filename << endl;
      return False;
    }

  int istrip;
  float deltaT;
  float slewing_a;
  float slewing_b;
  float mean_dz_plus;
  float mean_dz_minus;
  float mean_dphi_plus;
  float mean_dphi_minus;

  while (!fin.eof())
    {
      fin >> istrip;
      fin >> deltaT >> slewing_a >> slewing_b;
      fin >> mean_dz_plus >> mean_dz_minus;
      fin >> mean_dphi_plus >> mean_dphi_minus;

      set_DeltaT(istrip, deltaT);
      set_Slewing_A(istrip, slewing_a);
      set_Slewing_B(istrip, slewing_b);
      set_Mean_Dz_Plus(istrip, mean_dz_plus);
      set_Mean_Dz_Minus(istrip, mean_dz_minus);
      set_Mean_Dphi_Plus(istrip, mean_dphi_plus);
      set_Mean_Dphi_Minus(istrip, mean_dphi_minus);
    }
  
  cout << endl << "Fetched stripoff from " << filename 
       << " [ASCII file] " <<  endl;
  fin.close();
  
  return True;
}


PHBoolean TofwCalib::fetchrunoff(const char *filename)
{
  ifstream fin(filename);
  if(!fin)
    {
      cout << PHWHERE << " can't open " << filename << endl;
      return False;
    }
  
  int runnumber;
  float deltatRun;

  while (!fin.eof())
    {
      nruncalib++;
      fin>>runnumber>>deltatRun;
      set_runoffset(deltatRun);
    }
  cout << endl << "Fetched Toffset from " << filename 
       << " [ASCII file] " <<  endl;
  fin.close();
  
  return True;
}


PHBoolean TofwCalib::updateTvcConv(const int beginrun, const int endrun)
{

  RunToTime* runTime = RunToTime::instance();
  PHTimeStamp *ts(runTime->getBeginTime(beginrun));
  PHTimeStamp Tstart = *ts;
  delete ts;
  PHTimeStamp Tstop;
  
  // The runnumber is encoded into PHTimeStamp.
  if (endrun > 0)
    {
      ts = runTime->getEndTime(endrun);
      Tstop = *ts;
      delete ts;
    }
  else
    {
      Tstop.setToFarFuture();
    }

  //  Make the managers...
  PdbBankManager *bankManager = PdbBankManager::instance();
  PdbApplication *application = bankManager->getApplication();

  if(!application->startUpdate())
    {
      PHMessage("TofwCalib::updateTvcConv", PHError, 
		"Aborting... Database not writable");
      application->abort();
    }
  else if(application->startUpdate())
    {
      //  Make a bank ID...
      PdbBankID bankID;
      bankID.setInternalValue(1);
      const char* dataName = "calib.tofw.tdc";
      const char *descrip = "TvcConv: submitted by shengli huang";
      
      //  Grap a pointer to the bank...
      PdbCalBank *tofwBank = 
	bankManager->createBank("PdbParameterBank", bankID, descrip, Tstart, Tstop, dataName);
      
      int length = TOFW_NCH_TOTAL + 2; // array + 2 hdr
      tofwBank->setLength(length);
      
      PdbParameter *parameter;
      int index = 0;
      parameter = (PdbParameter *) & tofwBank->getEntry(index++);
      parameter->setParameter(1.0);
      parameter->setName("scheme");
      
      parameter = (PdbParameter *) & tofwBank->getEntry(index++);
      parameter->setParameter(length - 2);
      parameter->setName("entries");
      
      for (int i = 0; i < TOFW_NCH_TOTAL; i++)
	{
	  parameter = (PdbParameter *) & tofwBank->getEntry(index++);
	  int ibox = int(i/256);
	  int ich  = int(i%256);
	  if(ibox==0)      parameter->setParameter(TVC_ConvSouthBottom[ich]);
	  else if(ibox==1) parameter->setParameter(TVC_ConvNorthBottom[ich]);
	  else if(ibox==2) parameter->setParameter(TVC_ConvSouthTop[ich]);
	  else if(ibox==3) parameter->setParameter(TVC_ConvNorthTop[ich]);
	  parameter->setName("tvcconv");
	}
      
      if(debug) 
	{
	  tofwBank->print();
	  tofwBank->printHeader();
	  for (int i = 0; i < 3; i++) tofwBank->printEntry(i);
	}
      
      application->commit();
      cout << "Update TOFW TvcConv parameter DONE !!!" << endl;
    }
  
  return True;
}

PHBoolean TofwCalib::updateToffset(const int beginrun, const int endrun)
{

  RunToTime* runTime = RunToTime::instance();
  PHTimeStamp *ts(runTime->getBeginTime(beginrun));
  PHTimeStamp Tstart = *ts;
  delete ts;
  PHTimeStamp Tstop;
  
  // The runnumber is encoded into PHTimeStamp.
  if (endrun > 0)
    {
      ts = runTime->getEndTime(endrun);
      Tstop = *ts;
      delete ts;
    }
  else
    {
      Tstop.setToFarFuture();
    }

  //  Make the managers...
  PdbBankManager *bankManager = PdbBankManager::instance();
  PdbApplication *application = bankManager->getApplication();

  if(!application->startUpdate())
    {
      PHMessage("TofwCalib::updateToffset", PHError, 
		"Aborting... Database not writable");
      application->abort();
    }
  else if(application->startUpdate())
    {
      //  Make a bank ID...
      PdbBankID bankID;
      bankID.setInternalValue(1);
      const char* dataName = "calib.tofw.Toffset";
      const char *descrip = "Toffset: submitted by Shengli Huang";
      
      //  Grap a pointer to the bank...
      PdbCalBank *tofwBank = 
	bankManager->createBank("PdbParameterBank", bankID, descrip, Tstart, Tstop, dataName);
      
      int length = TOFW_NSTRIP_TOTAL + 2; // array + 2 hdr
      tofwBank->setLength(length);
      
      PdbParameter *parameter;
      int index = 0;
      parameter = (PdbParameter *) & tofwBank->getEntry(index++);
      parameter->setParameter(1.0);
      parameter->setName("scheme");
      
      parameter = (PdbParameter *) & tofwBank->getEntry(index++);
      parameter->setParameter(length - 2);
      parameter->setName("entries");
      
      for (int i = 0; i < TOFW_NSTRIP_TOTAL; i++)
	{
	  parameter = (PdbParameter *) & tofwBank->getEntry(index++);
	  parameter->setParameter(toffset[i]);
	  parameter->setName("toffset");
	}
      
      if(debug) 
	{
	  tofwBank->print();
	  tofwBank->printHeader();
	  for (int i = 0; i < 3; i++) tofwBank->printEntry(i);
	}
      
      application->commit();
      cout << "Update TOFW Toffset parameter DONE !!!" << endl;
    }
  
  return True;
}


PHBoolean TofwCalib::updatestripoff(const int beginrun, const int endrun)
{

  RunToTime* runTime = RunToTime::instance();
  PHTimeStamp *ts(runTime->getBeginTime(beginrun));
  PHTimeStamp Tstart = *ts;
  delete ts;
  PHTimeStamp Tstop;
  
  // The runnumber is encoded into PHTimeStamp.
  if (endrun > 0)
    {
      ts = runTime->getEndTime(endrun);
      Tstop = *ts;
      delete ts;
    }
  else
    {
      Tstop.setToFarFuture();
    }

  //  Make the managers...
  PdbBankManager *bankManager = PdbBankManager::instance();
  PdbApplication *application = bankManager->getApplication();

  if(!application->startUpdate())
    {
      PHMessage("TofwCalib::updatestripoff", PHError, 
		"Aborting... Database not writable");
      application->abort();
    }
  else if(application->startUpdate())
    {
      //  Make a bank ID...
      PdbBankID bankID;
      bankID.setInternalValue(1);
      const char* dataName = "calib.tofw.stripoff";
      const char *descrip = "stripoff: submitted by Shengli Huang";
      
      //  Grap a pointer to the bank...
      PdbCalBank *tofwBank = 
	bankManager->createBank("PdbParameterBank", bankID, descrip, Tstart, Tstop, dataName);
      
      int length = TOFW_NSTRIP_TOTAL*7 + 8 + 2; // array + 2 hdr
      tofwBank->setLength(length);
      
      PdbParameter *parameter;
      int index = 0;
      parameter = (PdbParameter *) & tofwBank->getEntry(index++);
      parameter->setParameter(1.0);
      parameter->setName("scheme");
      
      parameter = (PdbParameter *) & tofwBank->getEntry(index++);
      parameter->setParameter(length - 2);
      parameter->setName("entries");

      for (int i = 0; i < 4; i++)
	{
	  if(debug>0) cout<<sigma_dphi_par[i]<<" "<<sigma_dz_par[i]<<endl;

	  parameter = (PdbParameter *) & tofwBank->getEntry(index++);
	  parameter->setParameter(sigma_dphi_par[i]);
	  parameter->setName("matching_dphi");

	  parameter = (PdbParameter *) & tofwBank->getEntry(index++);
	  parameter->setParameter(sigma_dz_par[i]);
	  parameter->setName("matching_dz");
	}
      
      for (int i = 0; i < TOFW_NSTRIP_TOTAL; i++)
	{
	  if(debug>0) cout<<i<<" "<<DeltaT[i]<<" "<<Slewing_A[i]<<" "<<Mean_Dz_Plus[i]<<endl;
	  parameter = (PdbParameter *) & tofwBank->getEntry(index++);
	  parameter->setParameter(DeltaT[i]);
	  parameter->setName("DeltaT");

	  parameter = (PdbParameter *) & tofwBank->getEntry(index++);
	  parameter->setParameter(Slewing_A[i]);
	  parameter->setName("Slewing_A");

	  parameter = (PdbParameter *) & tofwBank->getEntry(index++);
	  parameter->setParameter(Slewing_B[i]);
	  parameter->setName("Slewing_B");

	  parameter = (PdbParameter *) & tofwBank->getEntry(index++);
	  parameter->setParameter(Mean_Dz_Plus[i]);
	  parameter->setName("Mean_Dz_Plus");

	  parameter = (PdbParameter *) & tofwBank->getEntry(index++);
	  parameter->setParameter(Mean_Dz_Minus[i]);
	  parameter->setName("Mean_Dz_Minus");

	  parameter = (PdbParameter *) & tofwBank->getEntry(index++);
	  parameter->setParameter(Mean_Dphi_Plus[i]);
	  parameter->setName("Mean_Dphi_Plus");

	  parameter = (PdbParameter *) & tofwBank->getEntry(index++);
	  parameter->setParameter(Mean_Dphi_Minus[i]);
	  parameter->setName("Mean_Dphi_Minus");
	}
      
      if(debug) 
	{
	  tofwBank->print();
	  tofwBank->printHeader();
	  for (int i = 0; i < 3; i++) tofwBank->printEntry(i);
	}
      
      application->commit();
      cout << "Update TOFW stripoff parameter DONE !!!" << endl;
    }
  
  return True;
}


PHBoolean TofwCalib::updaterunoff(const int beginrun, const int endrun)
{

  RunToTime* runTime = RunToTime::instance();
  PHTimeStamp *ts(runTime->getBeginTime(beginrun));
  PHTimeStamp Tstart = *ts;
  delete ts;
  PHTimeStamp Tstop;
  
  // The runnumber is encoded into PHTimeStamp.
  if (endrun > 0)
    {
      ts = runTime->getEndTime(endrun);
      Tstop = *ts;
      delete ts;
    }
  else
    {
      Tstop.setToFarFuture();
    }

  //  Make the managers...
  PdbBankManager *bankManager = PdbBankManager::instance();
  PdbApplication *application = bankManager->getApplication();

  if(!application->startUpdate())
    {
      PHMessage("TofwCalib::updaterunoff", PHError, 
		"Aborting... Database not writable");
      application->abort();
    }
  else if(application->startUpdate())
    {
      //  Make a bank ID...
      PdbBankID bankID;
      bankID.setInternalValue(1);
      const char* dataName = "calib.tofw.runoff";
      const char *descrip = "runoff: submitted by Shengli Huang";
      
      //  Grap a pointer to the bank...
      PdbCalBank *tofwBank = 
	bankManager->createBank("PdbParameterBank", bankID, descrip, Tstart, Tstop, dataName);
      
      int length = 2 + 2; // array + 2 hdr
      tofwBank->setLength(length);
      
      PdbParameter *parameter;
      int index = 0;
      parameter = (PdbParameter *) & tofwBank->getEntry(index++);
      parameter->setParameter(1.0);
      parameter->setName("scheme");
      
      parameter = (PdbParameter *) & tofwBank->getEntry(index++);
      parameter->setParameter(length - 2);
      parameter->setName("entries");
      
      //for (int i = 0; i < 2; i++)
      //{
      parameter = (PdbParameter *) & tofwBank->getEntry(index++);
      parameter->setParameter(beginrun);
      parameter->setName("RunNumber");
      
      parameter = (PdbParameter *) & tofwBank->getEntry(index++);
      parameter->setParameter(DeltaTRun);
      parameter->setName("DeltaTRun");
      //}
      
      if(debug) 
	{
	  tofwBank->print();
	  tofwBank->printHeader();
	  for (int i = 0; i < 3; i++) tofwBank->printEntry(i);
	}
      
      application->commit();
      cout << "Update TOFW runoff parameter DONE !!!" << endl;
    }
  
  return True;
}

PHBoolean TofwCalib::updaterunoff(const char *filename)
{
  ifstream fin(filename);
  if(!fin)
    {
      cout << PHWHERE << " can't open " << filename << endl;
      return False;
    }
  
  int runnumber;
  float deltatRun;

  while (!fin.eof())
    {
      fin>>runnumber>>deltatRun;
      
      set_runoffset(deltatRun);
      updaterunoff(runnumber);
    }
  cout << endl << "Fetched Toffset from " << filename 
       << " [ASCII file] " <<  endl;
  fin.close();
  
  return True;
}
