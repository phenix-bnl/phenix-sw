#include <iostream>
#include <cmath>
//INCLUDECHECKER: Removed this line: #include <cstdio>

#include "BbcCalib.hh"

using namespace std;

BbcCalib::BbcCalib(int version): CutVal(4), fittype(1)
{
  CalibrationVersion = version;
  for (int i = 0;i < 15;i++)
    {
      Version[i] = version;
    }

  Simulation = 1; // Calibration parameters for Simulation
  // after 07/03/01

  restore();
}

void
BbcCalib::getSimuInfo()
{
  if ( Simulation == 0 )
    {
      cout << "Load Calibration Parameters for simulation befor 07/03/01" 
	   << endl;
    }
  else if ( Simulation == 1 )
    {
      cout << "Load Calibration Parameters for simulation after 07/03/01" 
	   << endl;
    }
  else if ( Simulation == 2 )
    {
      cout << "PRDFtoDST option in preco is selected for Y2 simulation" 
	   << endl;
    }
  else {
      cout << "### Simulation FLAG not valid ####  Simulation=" 
	   << Simulation << " ####" << endl;
  }
}

float
BbcCalib::getCharge(int PmtIndx, int ADC)
{
  float Pedestal = (cpedestal.getCalibPar(PmtIndx))->getPeakChannel();
  float PmtGain = (cpmtgain.getCalibPar(PmtIndx))->getPeakChannel();
  float AdcGain = (cadcgain.getCalibPar(PmtIndx))->getPar1();

  if (PmtGain < 0.0001)
    {
      return 0.0;
    }

  return (ADC - Pedestal) * AdcGain / PmtGain;
}

float
BbcCalib::getHitTime0(int PmtIndx, int TDC, int ADC)
{
  float Pedestal = (cpedestal.getCalibPar(PmtIndx))->getPeakChannel();
  float PmtGain = (cpmtgain.getCalibPar(PmtIndx))->getPeakChannel();
  float OverFlow = (coverflow0.getCalibPar(PmtIndx))->getPeakChannel();
  float Sigma = (coverflow0.getCalibPar(PmtIndx))->getDeviation();
  float TdcGain = (ctdcgain0.getCalibPar(PmtIndx))->getPar1();

  float SlewPar[4];
  SlewPar[0] = (cslewing0.getCalibPar(PmtIndx))->getPar0();
  SlewPar[1] = (cslewing0.getCalibPar(PmtIndx))->getPar1();
  SlewPar[2] = (cslewing0.getCalibPar(PmtIndx))->getPar2();
  SlewPar[3] = (cslewing0.getCalibPar(PmtIndx))->getPar3();

  float timing = 0.0;
  if (PmtGain < 0.0001)
    {
      return -999.0;
    }
  double TrueAdc = ADC - Pedestal;
  if ((TDC > 0)
      && (TDC < (OverFlow - CutVal * Sigma))
      && (TrueAdc >= 1))
    {
      if (fittype == 0)
        {
          // base   : function [ f(x) = a + b/sqrt(x) ]
          timing = TDC * TdcGain + SlewPar[0] + SlewPar[1] / sqrt(TrueAdc);
        }
      else if (fittype == 1)
        {
          // extend : function [ f(x) = a + b/(x) + c*ln(x) ]
          timing = TDC * TdcGain + 
	    (SlewPar[0] + (SlewPar[1] / TrueAdc) + SlewPar[2] * log(TrueAdc));
        }
      else if (fittype == 2)
        {
          // extend : function [ f(x) = a + b/(x) + c/(x*x) + d*ln(x) ]
          timing = TDC * TdcGain + 
	    (SlewPar[0] + (SlewPar[1]/TrueAdc) + SlewPar[2]*log(TrueAdc) + SlewPar[3]/(TrueAdc*TrueAdc) );
        }
      else
        {
          cout << "Invalid fittype " << fittype << endl;
          timing = -9999.0;
        }
      return timing;
    }
  if (TDC < 0)
    {
      return TDC - 80000;
    }
  if (TDC > (OverFlow - 4 * Sigma))
    {
      return TDC - 90000;
    }
  if (TrueAdc < 1)
    {
      return TrueAdc - 200000;
    }

  return -9999.0;
}

float
BbcCalib::getHitTime1(int PmtIndx, int TDC, int ADC)
{
  float Pedestal = (cpedestal.getCalibPar(PmtIndx))->getPeakChannel();
  float PmtGain = (cpmtgain.getCalibPar(PmtIndx))->getPeakChannel();
  float OverFlow = (coverflow1.getCalibPar(PmtIndx))->getPeakChannel();
  float Sigma = (coverflow1.getCalibPar(PmtIndx))->getDeviation();
  float TdcGain = (ctdcgain1.getCalibPar(PmtIndx))->getPar1();

  float SlewPar[4];
  SlewPar[0] = (cslewing1.getCalibPar(PmtIndx))->getPar0();
  SlewPar[1] = (cslewing1.getCalibPar(PmtIndx))->getPar1();
  SlewPar[2] = (cslewing1.getCalibPar(PmtIndx))->getPar2();
  SlewPar[3] = (cslewing1.getCalibPar(PmtIndx))->getPar3();

  float timing = 0.0;

  if (PmtGain < 0.0001)
    {
      return -999.0;
    }
  double TrueAdc = ADC - Pedestal;
  if (TDC < (OverFlow - 4 * Sigma) && TrueAdc >= 1)
    {
      if (fittype == 0)
        {
          // base   : function [ f(x) = a + b/sqrt(x) ]
          timing = TDC * TdcGain + SlewPar[0] + SlewPar[1] / sqrt(TrueAdc);
        }
      else if (fittype == 1)
        {
          // extend : function [ f(x) = a + b/(x) + c*ln(x) ]
          timing = TDC * TdcGain + 
	    (SlewPar[0] + (SlewPar[1] / TrueAdc) + SlewPar[2] * log(TrueAdc));
        }
      else if (fittype == 2)
        {
          // extend : function [ f(x) = a + b/(x) + c/(x*x) + d*ln(x) ]
          timing = TDC * TdcGain + 
	    (SlewPar[0] + (SlewPar[1]/TrueAdc) + SlewPar[2]*log(TrueAdc) + SlewPar[3]/(TrueAdc*TrueAdc) );
        }
      else
        {
          cout << "Invalid fittype " << fittype << endl;
          timing = -9999.0;
        }
      return timing;
    }

  return -999.0;
}

float
BbcCalib::getAdc(int PmtIndx, int ADC)
{
  float Pedestal = (cpedestal.getCalibPar(PmtIndx))->getPeakChannel();

  return ADC - Pedestal;
}


float
BbcCalib::getArmHitTime(Bbc::ArmType arm, float ArmHitTimeOrg )
{
  float offset = (coffset.getCalibPar(arm))->getPeakChannel();
  if ( ArmHitTimeOrg != -9999.0 )
    {
      return ArmHitTimeOrg - offset;
    }

  return -9999.0;
}

float
BbcCalib::getCorrectedTzero( float TimingSouth, float TimingNorth )
{
  float TZeroOffset = (ctzerooff.getCalibPar(0))->getPeakChannel();
  if ( TimingSouth != -9999.0 && TimingNorth != -9999.0 )
    {
      return (TimingSouth + TimingNorth) / 2.0 - TZeroOffset;
    }

  return -9999.0;
}


int
BbcCalib::getPmtNumber( int femid, int femch )
{
  int find = 0;
  int ipmt = 0;

  for (ipmt = 0;ipmt < 128;ipmt++ )
    {
      int id0 = (config.getCalibPar(ipmt))->getFemID();
      int id1 = (config.getCalibPar(ipmt))->getFemChanID();
      if ( id0 == femid && id1 == femch )
        {
          find = 1;
          break;
        }
    }

  if ( find == 1 )
    {
      //      char* Arm = (config.getCalibPar(ipmt))->getArmID();
      char* Arm;
      const char *t = (config.getCalibPar(ipmt))->getArmID();
      char tc[1000];
      strcpy(tc,t);
      Arm = tc;
      
      int SNstatus = strcmp("S", Arm);
      if ( SNstatus == 0 )
        {
          return (config.getCalibPar(ipmt))->getPmtID();
        }
      else
        {
          return (config.getCalibPar(ipmt))->getPmtID() + 64;
        }
    }

  cout << "FemID = " << femid << "; FemChanID = " << femch << endl;
  cout << "There is no such configration in current BBC config." << endl;
  cout << "Please update configuration DB." << endl;
  return -1;
}

int
BbcCalib::getPmtIDinPISA( int ipmt )
{
  return (config.getCalibPar(ipmt))->getPmtIDinPIAS();
}

void
BbcCalib::showConfig()
{
  for (int i=0; i < 128; i++) {
          cout << (config.getCalibPar(i))->getPmtID () << " ";
          cout << (config.getCalibPar(i))->getFemID () << " ";
          cout << (config.getCalibPar(i))->getFemChanID () << " ";
          cout << (config.getCalibPar(i))->getHVgroupID () << " ";
          cout << (config.getCalibPar(i))->getHV () << " ";
          cout << (config.getCalibPar(i))->getFiberID () << " ";
          cout << (config.getCalibPar(i))->getPmtIDinPIAS () << " ";
          cout << (config.getCalibPar(i))->getArmID () << " ";
          cout << (config.getCalibPar(i))->getPmtSerialID () << endl;
  }
}

void
BbcCalib::showParameters()
{
  printf(" ### Loaded calibration parameters ###\n");
  for (int PmtIndx = 0; PmtIndx < 128; PmtIndx++ ) {
      if (getVersionForSlewing0()>=0&&getVersionForSlewing0() < 1000) {
          printf(" Ped:%7.2f OverFlow0:%7.2f Slew:%6.2f %6.2f Gain:%7.2f TdcConv:%5.3f AdcConv:%5.3f\n",
                 (cpedestal.getCalibPar(PmtIndx))->getPeakChannel(),
                 (coverflow0.getCalibPar(PmtIndx))->getPeakChannel(),
                 (cslewing0.getCalibPar(PmtIndx))->getPar0(),
                 (cslewing0.getCalibPar(PmtIndx))->getPar1(),
                 (cpmtgain.getCalibPar(PmtIndx))->getPeakChannel(),
                 (ctdcgain0.getCalibPar(PmtIndx))->getPar1(),
                 (cadcgain.getCalibPar(PmtIndx))->getPar1() );
      } else if (getVersionForSlewing0() < 3000) {
          printf(" Ped:%7.2f OverFlow0:%7.2f Slew:%6.2f %6.2f %6.2f Gain:%7.2f TdcConv:%5.3f AdcConv:%5.3f\n",
                 (cpedestal.getCalibPar(PmtIndx))->getPeakChannel(),
                 (coverflow0.getCalibPar(PmtIndx))->getPeakChannel(),
                 (cslewing0.getCalibPar(PmtIndx))->getPar0(),
                 (cslewing0.getCalibPar(PmtIndx))->getPar1(),
                 (cslewing0.getCalibPar(PmtIndx))->getPar2(),
                 (cpmtgain.getCalibPar(PmtIndx))->getPeakChannel(),
                 (ctdcgain0.getCalibPar(PmtIndx))->getPar1(),
                 (cadcgain.getCalibPar(PmtIndx))->getPar1() );
      } else {
          printf(" Ped:%7.2f OverFlow0:%7.2f Slew:%6.2f %6.2f %6.2f Gain:%7.2f TdcConv:%5.3f AdcConv:%5.3f FakePed(MC):%7.2f Theshold(MC):%7.2f TimeReso(MC):%5.3f\n",
                 (cpedestal.getCalibPar(PmtIndx))->getPeakChannel(),
                 (coverflow0.getCalibPar(PmtIndx))->getPeakChannel(),
                 (cslewing0.getCalibPar(PmtIndx))->getPar0(),
                 (cslewing0.getCalibPar(PmtIndx))->getPar1(),
                 (cslewing0.getCalibPar(PmtIndx))->getPar2(),
                 (cpmtgain.getCalibPar(PmtIndx))->getPeakChannel(),
                 (ctdcgain0.getCalibPar(PmtIndx))->getPar1(),
                 (cadcgain.getCalibPar(PmtIndx))->getPar1(),
                 (cfakeped.getCalibPar(PmtIndx))->getPeakChannel(),
                 (cthreshold.getCalibPar(PmtIndx))->getPeakChannel(),
                 (ctimereso.getCalibPar(PmtIndx))->getPeakChannel() );
      }
  }
  cout << "Offset For South BBC  = " << (coffset.getCalibPar(0))->getPeakChannel() << " ";
  cout << "Offset For North BBC  = " << (coffset.getCalibPar(1))->getPeakChannel() << " ";
  cout << "Time Zero Offset      = " << (ctzerooff.getCalibPar(0))->getPeakChannel() << endl;
  cout << endl;
  cout << "Function type " << endl;
  if (getFunctionType() == 0) {
    cout << " a+(b/sqrt(ADC))" << endl;
  } else if (getFunctionType() == 1) {
    cout << " a+(b/ADC)+c*log(ADC)" << endl;
  } else {
    cout << "ERROR :: Invalid function type " << endl;
  }

  showParameterVersion();
}

void
BbcCalib::WriteLvl2calibs()
{
  if (getVersionForSlewing0() < 1000) {
    cout << "BbcCalib::WriteLvl2Calibs: wrong version for slewing" << endl;
    return;
  }

  float factor =1.0;
  cout << factor << endl;
  for (int PmtIndx = 0;PmtIndx<128;PmtIndx++)
    {
      cout << PmtIndx << " " 
	   << (cpedestal.getCalibPar(PmtIndx))->getPeakChannel() << " "
	   << (coverflow0.getCalibPar(PmtIndx))->getPeakChannel() << " "
	   << (coverflow1.getCalibPar(PmtIndx))->getPeakChannel() << " "
	   << (cadcgain.getCalibPar(PmtIndx))->getPar1()  << " "
	   << (cpmtgain.getCalibPar(PmtIndx))->getPeakChannel()  << " "
	   << (ctdcgain0.getCalibPar(PmtIndx))->getPar1()  << " "
	   << (ctdcgain1.getCalibPar(PmtIndx))->getPar1()  << " "
	   << (cslewing0.getCalibPar(PmtIndx))->getPar0() << " "
	   << (cslewing0.getCalibPar(PmtIndx))->getPar1() << " "
	   << (cslewing0.getCalibPar(PmtIndx))->getPar2() << " "
	   << (cslewing1.getCalibPar(PmtIndx))->getPar0() << " "
	   << (cslewing1.getCalibPar(PmtIndx))->getPar1() << " "
	   << (cslewing1.getCalibPar(PmtIndx))->getPar2() << " "
	   << (coverflow0.getCalibPar(PmtIndx))->getDeviation() << " "
	   << endl;
    }

  cout << (coffset.getCalibPar(0))->getPeakChannel() << " "
       << (coffset.getCalibPar(1))->getPeakChannel() << " "
       << (ctzerooff.getCalibPar(0))->getPeakChannel() << endl;

  return;

}

int 
BbcCalib::restore(const PHTimeStamp& time, int version)
{
  CalibrationVersion = version;
  int status;

  setVersionAll(version);
  status = coverflow0.restore(time, "overflow0", getVersionForOverflow0());
  status += coverflow1.restore(time, "overflow1", getVersionForOverflow1());
  status += cpedestal.restore( time, "pedestal" , getVersionForPedestal() );
  status += cpmtgain.restore( time, "pmtgain" , getVersionForPmtGain() );
  status += coffset.restore( time, "offset" , getVersionForOffset() );
  status += ctzerooff.restore( time, "tzero" , getVersionForTZeroOff() );
  status += cadcgain.restore( time, "adc" , getVersionForAdcGain() );
  status += ctdcgain0.restore( time, "tdc0" , getVersionForTdcGain0() );
  status += ctdcgain1.restore( time, "tdc1" , getVersionForTdcGain1() );
  status += cslewing0.restore( time, "slewpar0" , getVersionForSlewing0() );
  status += cslewing1.restore( time, "slewpar1" , getVersionForSlewing1() );
  status += config.restore( time, "config" , getVersionForConfig() );

  // Version containing simulation constants begins from 3000
  if (CalibrationVersion >= 3000) {
    status += cfakeped.restore( time, "fakeped" , getVersionForFakePed() );
    status += cthreshold.restore( time, "threshold" , getVersionForThreshold() );
    status += ctimereso.restore( time, "timereso" , getVersionForTimeReso() );
  }

  return status;
}

int
BbcCalib::restore(const BbcTime_t& time)
{
  int status = 0;
  int version = 0; // 0 for function type 0, 1000 for function type 1

  if (time < PHTimeStamp(2001, 1, 1, 0, 0, 0))
    {
      // default version of Y1 is function type 0 and calibration verion 0.
      version=0;
    }
  else
    {
      version=CalibrationVersion;
    }
  status = restore(time, version);

  return status;
}

int 
BbcCalib::restore(const char *filename, int version)
{
  CalibrationVersion = version;
  int status;

  setVersionAll(version);
  status = restore(filename);

  return status;
}

int
BbcCalib::restore(const char* filename)
{
  PHString InFile = filename;
  PHString f00 = ".overflow0";
  PHString f01 = ".overflow1";
  PHString f02 = ".pedestal";
  PHString f03 = ".pmtgain";
  PHString f04 = ".offset";
  PHString f05 = ".tzero";
  PHString f06 = ".adc";
  PHString f07 = ".tdc0";
  PHString f08 = ".tdc1";
  PHString f09 = ".slewpar0";
  PHString f10 = ".slewpar1";
  PHString f11 = ".config";
  PHString f12 = ".fakeped";
  PHString f13 = ".threshold";
  PHString f14 = ".timereso";

  PHString name00 = InFile + f00;
  PHString name01 = InFile + f01;
  PHString name02 = InFile + f02;
  PHString name03 = InFile + f03;
  PHString name04 = InFile + f04;
  PHString name05 = InFile + f05;
  PHString name06 = InFile + f06;
  PHString name07 = InFile + f07;
  PHString name08 = InFile + f08;
  PHString name09 = InFile + f09;
  PHString name10 = InFile + f10;
  PHString name11 = InFile + f11;
  PHString name12 = InFile + f12;
  PHString name13 = InFile + f13;
  PHString name14 = InFile + f14;

  coverflow0.restore(name00.getString(), "overflow0");
  coverflow1.restore(name01.getString(), "overflow1");
  cpedestal.restore( name02.getString(), "pedestal");
  cpmtgain.restore( name03.getString(), "pmtgain");
  coffset.restore( name04.getString(), "offset");
  ctzerooff.restore( name05.getString(), "tzero");
  cadcgain.restore( name06.getString(), "adc");
  ctdcgain0.restore( name07.getString(), "tdc0");
  ctdcgain1.restore( name08.getString(), "tdc1");
  cslewing0.restore( name09.getString(), "slewpar0");
  cslewing1.restore( name10.getString(), "slewpar1");
  config.restore( name11.getString(), "config");

  // Version containing simulation constants begins from 3000
  if (CalibrationVersion >= 3000) {
    cfakeped.restore( name12.getString(), "fakeped");
    cthreshold.restore( name13.getString(), "threshold");
    ctimereso.restore( name14.getString(), "timereso");
  }

  showParameterVersion();

  return 0;
}

int
BbcCalib::restore()
{
  coverflow0.restore("overflow0");
  coverflow1.restore("overflow1");
  cpedestal.restore("pedestal");
  cpmtgain.restore("pmtgain");
  coffset.restore("offset");
  ctzerooff.restore("tzero");
  cadcgain.restore("adc");
  ctdcgain0.restore("tdc0");
  ctdcgain1.restore("tdc1");
  cslewing0.restore("slewpar0");
  cslewing1.restore("slewpar1");
  config.restore("config");

  // Version containing simulation constants begins from 3000
  if (CalibrationVersion >= 3000) {
    cfakeped.restore("fakeped");
    cthreshold.restore("threshold");
    ctimereso.restore("timereso");
  }

  if ( Simulation == 0 )
    {
      int i;
      for (i = 0; i < 64; i++)
        {
          PHString tmp1 ("S");
          PHString tmp2 ("N/A");

          (config.getCalibPar(i))->setPmtID (i + 1);
          (config.getCalibPar(i))->setFemID (i / 8 + 1);
          (config.getCalibPar(i))->setFemChanID (i % 8 + 1);
          (config.getCalibPar(i))->setHVgroupID (0);
          (config.getCalibPar(i))->setHV (0);
          (config.getCalibPar(i))->setFiberID (0);
          (config.getCalibPar(i))->setPmtIDinPIAS (i + 1);
          (config.getCalibPar(i))->setArmID (tmp1);
          (config.getCalibPar(i))->setPmtSerialID (tmp2);
        }
      for (i = 64; i < 128; i++)
        {
          PHString tmp1 ("N");
          PHString tmp2 ("N/A");

          (config.getCalibPar(i))->setPmtID (i - 64 + 1);
          (config.getCalibPar(i))->setFemID (i / 8 + 1);
          (config.getCalibPar(i))->setFemChanID (i % 8 + 1);
          (config.getCalibPar(i))->setHVgroupID (0);
          (config.getCalibPar(i))->setHV (0);
          (config.getCalibPar(i))->setFiberID (0);
          (config.getCalibPar(i))->setPmtIDinPIAS (i - 64 + 1);
          (config.getCalibPar(i))->setArmID (tmp1);
          (config.getCalibPar(i))->setPmtSerialID (tmp2);
        }


    }
  else if ( Simulation > 2 || Simulation < 0 )
    {
      cout << "Simulation flag not valid  ####  Simulation=" << Simulation 
	   << " ####" << endl;
    }

  return 0;
}

void 
BbcCalib::setFunctionType(int type)
{
  fittype = type;
}

void
BbcCalib::showParameterVersion()
{
  printf("######################################\n");
  printf("  Version for Calibration parameters  \n");
  printf("######################################\n");
  printf("For Overflow0    = Ver %f.3\n", getVersionForOverflow0() / 1000.);
  printf("For OverFlow1    = Ver %f.3\n", getVersionForOverflow1() / 1000.);
  printf("For Pedestal     = Ver %f.3\n", getVersionForPedestal() / 1000.);
  printf("For PmtGain      = Ver %f.3\n", getVersionForPmtGain() / 1000.);
  printf("For Arm Offset   = Ver %f.3\n", getVersionForOffset() / 1000.);
  printf("For Time Zero    = Ver %f.3\n", getVersionForTZeroOff() / 1000.);
  printf("For AdcGain      = Ver %f.3\n", getVersionForAdcGain() / 1000.);
  printf("For TdcGain0     = Ver %f.3\n", getVersionForTdcGain0() / 1000.);
  printf("For TdcGain1     = Ver %f.3\n", getVersionForTdcGain1() / 1000.);
  printf("For Slewing Par0 = Ver %f.3\n", getVersionForSlewing0() / 1000.);
  printf("For Slewing Par1 = Ver %f.3\n", getVersionForSlewing1() / 1000.);
  printf("For BBC Config   = Ver %f.3\n", getVersionForConfig() / 1000.);
  printf("For FakePedestal = Ver %f.3\n", getVersionForFakePed() / 1000.);
  printf("For Threshold    = Ver %f.3\n", getVersionForThreshold() / 1000.);
  printf("For TimeReso     = Ver %f.3\n", getVersionForTimeReso() / 1000.);
  printf("######################################");
}

void 
BbcCalib::setVersionAll(int version )
{
  CalibrationVersion = version;
  setVersionForPedestal(version);
  setVersionForOverflow0(version);
  setVersionForOverflow1(version);
  setVersionForPmtGain(version);
  setVersionForOffset(version);
  setVersionForTZeroOff(version);
  setVersionForAdcGain(version);
  setVersionForTdcGain0(version);
  setVersionForTdcGain1(version);
  setVersionForSlewing0(version);
  setVersionForSlewing1(version);
  setVersionForConfig(version);
  setVersionForFakePed(version);
  setVersionForThreshold(version);
  setVersionForTimeReso(version);

  if(version>=0&&version<1000){
    ////////////////////////////////////////////////
    // function type = a + b/sqrt(ADC)
    ////////////////////////////////////////////////
    setFunctionType(0);
  } else if(version>=1000){
    ////////////////////////////////////////////////
    // function type 1 : f(x) = a+(b/ADC)+c*log(ADC)
    ////////////////////////////////////////////////
    setFunctionType(1);
  } else {
    cout<<"Invalid Version Number = "<<version<<endl;
    setFunctionType(1);
  }
}
