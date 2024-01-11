#include <phool.h>
#include <MpcCalib.h>
#include <RunToTime.hh>
#include <recoConsts.h>
#include <mpchalf.h>
#include <RunNumberRanges.h>

// Database Includes
#include <PdbBankManager.hh>
#include <PdbApplication.hh>
#include <PdbBankID.hh>
#include <PdbCalBank.hh>

#include <PHString.h>
#include <PHCompositeNode.h>
#include <PHNodeIterator.h>
#include <PHIODataNode.h>
#include <RunHeader.h>
#include <getClass.h>


#include <TFile.h>
#include <TH2.h>

#include <cmath>
#include <cstdlib>
#include <cstring>
#include <fstream>
#include <iostream>

typedef PHIODataNode<PHObject> PHObjectNode_t;

using namespace std;

MpcCalib *MpcCalib::__instance = NULL;

MpcCalib *MpcCalib::instance(PHCompositeNode *topNode)
{
  if (__instance)
    {
      return __instance;
    }

  // instantiate new MpcCalib on first call
  __instance = new MpcCalib();

  if ( topNode!=0 )
    {
      __instance->AddToNodeTree(topNode);
    }

  return __instance;
}

MpcCalib::MpcCalib(PHCompositeNode *topNode, const int do_download)
{
  status = 0;
  Reset();

  if ( topNode!=0 )
    {
      this->AddToNodeTree(topNode);
    }

  if ( do_download==1 ) Download_All(topNode);

  __instance = this;
}

int MpcCalib::Download_All(PHCompositeNode *topNode)
{
  // get database location
  const char *dbase_directory = getenv("MPC_DATABASE");

  // if MPC_DATABASE is set, we read in the textfiles in the MPC_DATAVASE
  // directory
  if ( dbase_directory != NULL )
    {
      Download_All( dbase_directory );
    }
  else
    {
      int run_number = 0;
      recoConsts *rc = recoConsts::instance();

      // If MPC_DATABASE is not set, we get runnumber from recoConst RUNNUMBER
      // Otherwise, we get it from the RunHeader Object in the node tree.
      if ( rc->FlagExist("RUNNUMBER") )
        {
          // Download most recent map from database
          run_number = rc->get_IntFlag("RUNNUMBER");
        }
      else if ( topNode!=0 )
        {
	  RunHeader *runheader = findNode::getClass<RunHeader>(topNode, "RunHeader");
  	  if (runheader!=0)
  	    {
  	      run_number = runheader->get_RunNumber();
              cout << PHWHERE << " Getting RunNumber from RunHeader, " << run_number << endl;
  	    }
        }

      if ( run_number == 0 )
        {
          // Download most recent map from database
          cout << PHWHERE << " Warning, didn't find runnumber, downloading most recent calibs" << endl;
          Download_All( PHTimeStamp( time(NULL) ) );
        }
      else
        {
          RunToTime *runtotime = RunToTime::instance();
          PHTimeStamp *timestamp = runtotime->getBeginTime( run_number );
          cout << PHWHERE << " Downloading mpc calibs from run " << run_number << endl;
          Download_All( *timestamp , run_number  );
          delete timestamp;
        }
    }

  return 1;
}

MpcCalib::~MpcCalib()
{
 
/*
  // check for != 0 not needed, deleting null pointers is allowed
  // Now delete the leakage histograms
  for (int iarm=0; iarm<2; iarm++)
    {
      for (int ie=0; ie<4; ie++)
        {
              delete h2_leakage[iarm][ie];
        }
    }
*/
  __instance = 0;
}

int MpcCalib::Download_All(const string& dbase_directory)
{
  // MPC_DATABASE is set
  cout << "MpcCalib: Getting calibrations from " << dbase_directory << endl;
 
  status = 0;

  string dbase_location_gains(dbase_directory);
  dbase_location_gains += "/MpcCal.gains";
  status += Download_Gains(dbase_location_gains);

  string dbase_location_ratio(dbase_directory);
  dbase_location_ratio += "/MpcCal.hiloratio";
  status += Download_HiLoRatio(dbase_location_ratio);

  string dbase_location_limit(dbase_directory);
  dbase_location_limit += "/MpcCal.hilolimit";
  status += Download_HiLoLimit(dbase_location_limit);

  string dbase_location_oflow(dbase_directory);
  dbase_location_oflow += "/MpcCal.overflow";
  status += Download_TDC_Overflow(dbase_location_oflow);

  string dbase_location_lc(dbase_directory);
  dbase_location_lc += "/MpcCal.leastcount";
  status += Download_TDC_LeastCount(dbase_location_lc);

  string dbase_ped(dbase_directory);	// New HBD Pedestals
  dbase_ped += "/MpcCal.ped";
  status += Download_Pedestals(dbase_ped,"ped");

  string dbase_hgpre(dbase_directory);
  dbase_hgpre += "/MpcCal.hipreped";
  status += Download_Pedestals(dbase_hgpre,"hipre");

  string dbase_hgpost(dbase_directory);
  dbase_hgpost += "/MpcCal.hipostped";
  status += Download_Pedestals(dbase_hgpost,"hipost");

  string dbase_lgpre(dbase_directory);
  dbase_lgpre += "/MpcCal.lopreped";
  status += Download_Pedestals(dbase_lgpre,"lopre");

  string dbase_lgpost(dbase_directory);
  dbase_lgpost += "/MpcCal.lopostped";
  status += Download_Pedestals(dbase_lgpost,"lopost");

  string dbase_slewcorr(dbase_directory);
  dbase_slewcorr += "/MpcCal.slewcor";
  status += Download_SlewCorrection(dbase_slewcorr);

  string dbase_t0(dbase_directory);
  dbase_t0 += "/MpcCal.t0";
  status += Download_T0(dbase_t0);
  
  string dbase_pshape(dbase_directory);
  dbase_pshape += "/MpcCal.pshape";
  status += Download_Shapes(dbase_pshape,"p");
  
  string dbase_np1shape(dbase_directory);
  dbase_np1shape += "/MpcCal.np1shape";
  status += Download_Shapes(dbase_pshape,"np1");
  
/*
  string dbase_gaincorr(dbase_directory);
  dbase_gaincorr += "/MpcCal.gaincorr";
  status += Download_GainCorr(dbase_gaincorr);
*/
  
  //Download_LeakageCorrection(dbase_directory);
  return status;
}

int MpcCalib::Download_Gains(const string& dbase_location)
{
  // Reset All Values
  for (int ifee576ch=0; ifee576ch<MAXCH; ifee576ch++)
    {
      adc_gain[ifee576ch].setPeakChannel(0.);
      adc_gain[ifee576ch].setDeviation(0.);
      adc_gain[ifee576ch].setStatus(-1);
    }

  ifstream infile( dbase_location.c_str() );
  if ( !infile.is_open() )
    {
      cout << PHWHERE << "unable to open " << dbase_location << endl;
      status = -3;
      return status;
    }

  int   fee576ch;
  float gain;
  float gain_rms;
  int   gain_status;

  while ( infile >> fee576ch >> gain >> gain_rms >> gain_status )
    {
      adc_gain[fee576ch].setPeakChannel(gain);
      adc_gain[fee576ch].setDeviation(gain_rms);
      adc_gain[fee576ch].setStatus(gain_status);
    }

  return 1;
}

int MpcCalib::Download_GainCorr(const string& dbase_location)
{
  ifstream infile( dbase_location.c_str() );
  if ( !infile.is_open() )
    {
      cout << PHWHERE << "unable to open " << dbase_location << endl;
      status = -3;
      return status;
    }

  int   fee576ch;
  float par0;
  float par1;
  float par2;
  float par3;

  // change this to use getline, and count the number of words
  while ( infile >> fee576ch >> par0 >> par1 >> par2 >> par3 )
    {
      gain_corr[fee576ch].setVersion(1);
      gain_corr[fee576ch].setPar(0,par0);
      gain_corr[fee576ch].setPar(1,par1);
      gain_corr[fee576ch].setPar(2,par2);	// time in days
      gain_corr[fee576ch].setPar(3,par3);
      gain_corr[fee576ch].setChi2(-1.);
    }

  return 1;
}

int MpcCalib::Download_Led(const string& dbase_location)
{
  ifstream infile( dbase_location.c_str() );
  if ( !infile.is_open() )
    {
      cout << PHWHERE << "unable to open " << dbase_location << endl;
      status = -3;
      return status;
    }

  Short_t status = -1;
  Short_t ch = -999;
  Float_t mean = 1.;
  Float_t dmean = 0.;
  Float_t chi2 = 0.;
  Short_t ndf = 0;

  while ( infile >> status >> ch )
    {
      infile >> mean >> dmean >> chi2 >> ndf;

      if ( ch<0 || ch>=MAXCH )
        {
          cout << PHWHERE << " ERROR, found bad ch " << ch << endl;
          continue;
        }

      led[ch].set_status( status );
      led[ch].set_fee576ch( ch );
      led[ch].set_mean( mean );
      led[ch].set_dmean( dmean );
      led[ch].set_chisquare( chi2 );
      led[ch].set_ndf( ndf );
      //cout << "mpcledxxx " << led[ch].get_fee576ch() << "\t" << led[ch].get_mean() << endl;
    }
  infile.close();

  return 1;
}

int MpcCalib::Download_TDC_Overflow(const string& dbase_location)
{
  ifstream infile( dbase_location.c_str() );
  if ( !infile.is_open() )
    {
      cout << PHWHERE << "unable to open " << dbase_location << endl;
      status = -3;
      return status;
    }
  //char header[256];
  //infile.getline(header,256);

  int fee576ch = 0;
  float temp_tdc_oflow_mean;
  float temp_tdc_oflow_rms;
  int   temp_tdc_oflow_status;
  while ( infile >> fee576ch >> temp_tdc_oflow_mean >> temp_tdc_oflow_rms >> temp_tdc_oflow_status )
    {
      tdc_oflow[fee576ch].setPeakChannel(temp_tdc_oflow_mean);
      tdc_oflow[fee576ch].setDeviation(temp_tdc_oflow_rms);
      tdc_oflow[fee576ch].setStatus(temp_tdc_oflow_status);
    }
  infile.close();

  return 1;
}

int MpcCalib::Download_TDC_LeastCount(const string& dbase_location)
{
  ifstream infile( dbase_location.c_str() );
  if ( !infile.is_open() )
    {
      cout << PHWHERE << "unable to open " << dbase_location << endl;
      status = -3;
      return status;
    }
  //char header[256];
  //infile.getline(header,256);

  int fee576ch = 0;
  float temp_tdc_leastcount = 0.;
  float temp_tdc_leastcount_rms = 0.;
  int temp_tdc_leastcount_status = 0;
  while ( infile >> fee576ch >> temp_tdc_leastcount >> temp_tdc_leastcount_rms >> temp_tdc_leastcount_status )
    {
      tdc_leastcount[fee576ch].setPeakChannel(temp_tdc_leastcount);
      tdc_leastcount[fee576ch].setDeviation(temp_tdc_leastcount_rms);
      tdc_leastcount[fee576ch].setStatus(temp_tdc_leastcount_status);
    }

  return 1;
}

int MpcCalib::Download_Pedestals(const string& dbase_file, const string& type)
{

  ifstream infile( dbase_file.c_str() );
  if ( !infile.is_open() )
    {
      cout << PHWHERE << "unable to open " << dbase_file << endl;
      status = -3;
      return status;
    }

  int fee576ch = 0;
  int iamu = -1;
  float temp_ped;
  float temp_pedrms;
  int   temp_status;

  if ( type=="ped" )
    {
      // New HBD Electronics Pedestals
      while ( infile >> fee576ch >> temp_ped >> temp_pedrms >> temp_status )
        {
          ped[fee576ch].setPeakChannel(temp_ped);
          ped[fee576ch].setDeviation(temp_pedrms);
          ped[fee576ch].setStatus(temp_status);
        }
    }
  else
    {
      while ( infile >> fee576ch >> iamu >> temp_ped >> temp_pedrms >> temp_status )
        {
          if ( fee576ch<0 || fee576ch>575 )
            {
              cout << PHWHERE << " bad ch in " << type << "\t" << fee576ch << endl;
            }
          if ( iamu<0 || iamu>63 )
            {
              cout << PHWHERE << " bad amu in " << type << "\t" << iamu << endl;
            }
    
          if ( type=="lopre" )
            {
              lgpre[fee576ch*MAXAMU+iamu].setPeakChannel(temp_ped);
              lgpre[fee576ch*MAXAMU+iamu].setDeviation(temp_pedrms);
              lgpre[fee576ch*MAXAMU+iamu].setStatus(temp_status);
            }
          else if ( type=="lopost" )
            {
              lgpost[fee576ch*MAXAMU+iamu].setPeakChannel(temp_ped);
              lgpost[fee576ch*MAXAMU+iamu].setDeviation(temp_pedrms);
              lgpost[fee576ch*MAXAMU+iamu].setStatus(temp_status);
            }
          else if ( type=="hipre" )
            {
              hgpre[fee576ch*MAXAMU+iamu].setPeakChannel(temp_ped);
              hgpre[fee576ch*MAXAMU+iamu].setDeviation(temp_pedrms);
              hgpre[fee576ch*MAXAMU+iamu].setStatus(temp_status);
            }
          else if ( type=="hipost" )
            {
              hgpost[fee576ch*MAXAMU+iamu].setPeakChannel(temp_ped);
              hgpost[fee576ch*MAXAMU+iamu].setDeviation(temp_pedrms);
              hgpost[fee576ch*MAXAMU+iamu].setStatus(temp_status);
            }
        }

    }

  infile.close();
  return 1;
}

int MpcCalib::Download_HiLoRatio(const string& dbase_textfile)
{
  // Read in ratios
  ifstream infile( dbase_textfile.c_str() );
  if ( !infile.is_open() )
    {
      cout << PHWHERE << "unable to open " << dbase_textfile << endl;
      status = -3;
      return status;
    }

  int fee576ch;
  float ratio;
  float ratio_rms;
  int   ratio_status;

  while ( infile >> fee576ch >> ratio >> ratio_rms >> ratio_status )
    {
      hilo_ratio[fee576ch].setPeakChannel( ratio );
      hilo_ratio[fee576ch].setDeviation( ratio_rms );
      hilo_ratio[fee576ch].setStatus( ratio_status );
    }

  return 1;
}

int MpcCalib::Download_HiLoLimit(const string& dbase_textfile)
{
  // Now read in limits
  ifstream infile_hilolimit( dbase_textfile.c_str() );
  if ( !infile_hilolimit.is_open() )
    {
      cout << PHWHERE << "unable to open " << dbase_textfile << endl;
      status = -3;
      return status;
    }

  int   fee576ch;
  float limit;
  float limit_rms;
  int   limit_status;

  while ( infile_hilolimit >> fee576ch >> limit >> limit_rms >> limit_status )
    {
      hilo_limit[fee576ch].setPeakChannel( limit );
      hilo_limit[fee576ch].setDeviation( limit_rms );
      hilo_limit[fee576ch].setStatus( limit_status );
    }

  return 1;
}

int MpcCalib::Download_SlewCorrection(const string& dbase_textfile)
{
  ifstream infile_slewcor(dbase_textfile.c_str() );
  if ( !infile_slewcor.is_open() )
    {
      cout << PHWHERE << "unable to open " << dbase_textfile << endl;
      status = -3;
      return status;
    }
  int fee576ch;
  float value0, value1;
  while ( infile_slewcor >> fee576ch >> value0 >> value1)
    {
      slewcor1[fee576ch] = value1;
      slewcor0[fee576ch] = value0;      
    }
  infile_slewcor.close();
  return 1;
}

int MpcCalib::Download_T0(const string& dbase_textfile)
{
  ifstream infile_t0(dbase_textfile.c_str() );
  if ( !infile_t0.is_open() )
    {
      cout << PHWHERE << "unable to open " << dbase_textfile << endl;
      status = -3;
      return status;
    }
  int fee576ch;
  float value0;
  while ( infile_t0 >> fee576ch >> value0)
    t0[fee576ch] = value0;

  infile_t0.close();
  return 1;
}

int MpcCalib::Download_Shapes(const string& dbase_file, const string& type)
{
  PdbMpcShape *shapes = 0;
  PdbMpcShape *sherrs = 0;

  if ( type=="p" || type=="P" )
    {
      shapes = pshapes;
      sherrs = psherrs;
    }
  else if ( type=="np1" || type=="NP1" )
    {
      shapes = np1shapes;
      sherrs = np1sherrs;
    }

  ifstream infile( dbase_file.c_str() );
  if ( !infile.is_open() )
    {
      cout << PHWHERE << " ERROR, unable to open " << dbase_file << endl;
      status = -3;
      return status;
    }
  // Get the error file
  TString err_fname = dbase_file.c_str();
  err_fname.ReplaceAll("shape","sherr");
  ifstream errfile( err_fname.Data() );
  if ( !errfile.is_open() )
    {
      cout << PHWHERE << " ERROR, unable to open " << dbase_file << endl;
      status = -3;
      return status;
    }

  int fee576ch = 0;
  int   temp_nsamples;
  float temp_start_time;
  float temp_end_time;
  float temp_val;

  // Photonic Shape
  while ( infile >> fee576ch >> temp_nsamples >> temp_start_time >> temp_end_time )
    {
      //cout << "shape " << fee576ch << "\t" <<  temp_nsamples << "\t" <<  temp_start_time << "\t" <<  temp_end_time << endl;
      if ( fee576ch<0 || fee576ch>575 )
        {
          cerr << PHWHERE << " ERROR, feech is " << fee576ch << endl;
          return -1;
        }

      shapes[fee576ch].set_fee576ch( fee576ch );
      shapes[fee576ch].set_nsamples( temp_nsamples );
      shapes[fee576ch].set_start_time( temp_start_time );
      shapes[fee576ch].set_end_time( temp_end_time );

      for (int isamp=0; isamp<temp_nsamples; isamp++)
        {
          infile >> temp_val;
          shapes[fee576ch].add_float( temp_val );
          //cout << temp_val << " ";
          //if ( isamp%10==9 ) cout << endl;
        }
        //cout << endl;
    }

  // Now get the errors
  while ( errfile >> fee576ch >> temp_nsamples >> temp_start_time >> temp_end_time )
    {
      //cout << "sherr " << fee576ch << "\t" <<  temp_nsamples << "\t" <<  temp_start_time << "\t" <<  temp_end_time << endl;
      if ( fee576ch<0 || fee576ch>575 )
        {
          cerr << PHWHERE << " ERROR, feech is " << fee576ch << endl;
          return -1;
        }

      sherrs[fee576ch].set_fee576ch( fee576ch );
      sherrs[fee576ch].set_nsamples( temp_nsamples );
      sherrs[fee576ch].set_start_time( temp_start_time );
      sherrs[fee576ch].set_end_time( temp_end_time );

      for (int isamp=0; isamp<temp_nsamples; isamp++)
        {
          errfile >> temp_val;
          sherrs[fee576ch].add_float( temp_val );
          //cout << temp_val << " ";
          //if ( isamp%10==9 ) cout << endl;
        }
      //cout << endl << sherrs[fee576ch].getVector().size() << endl;
    }


  infile.close();
  return 1;
}

/*
int MpcCalib::Download_LeakageCorrection(const string& dbase_textfile)
{
  const int NUM_EBINS = 4;
  const char *filetags[2][NUM_EBINS] = {
      { "leakage/007.more_stats/output/ISrhh2_eres/south_0_5/leakage/ISrhh2_eres.root",
        "leakage/007.more_stats/output/ISrhh2_eres/south_5_10/leakage/ISrhh2_eres.root",
        "leakage/007.more_stats/output/ISrhh2_eres/south_10_20/leakage/ISrhh2_eres.root",
        "leakage/007.more_stats/output/ISrhh2_eres/south_20_30/leakage/ISrhh2_eres.root" },
      { "leakage/007.more_stats/output/ISrhh2_eres/north_0_5/leakage/ISrhh2_eres.root",
        "leakage/007.more_stats/output/ISrhh2_eres/north_5_10/leakage/ISrhh2_eres.root",
        "leakage/007.more_stats/output/ISrhh2_eres/north_10_20/leakage/ISrhh2_eres.root",
        "leakage/007.more_stats/output/ISrhh2_eres/north_20_30/leakage/ISrhh2_eres.root" }
    };

  int status = 0;

  cout << "Downloading Leakage Files ***" << endl;
  for (int iarm=0; iarm<2; iarm++)
    {
      for (int ienergy=0; ienergy<NUM_EBINS; ienergy++)
        {
	  string filename=dbase_textfile + "/" + filetags[iarm][ienergy];
          cout << "arm " << iarm << " ebin " << ienergy << "\t" << filename << endl;
          TFile *infile = new TFile(filename.c_str(),"READ");
          h2_leakage[iarm][ienergy] = (TH2*)infile->Get("leakage_mean_offset");
          if ( h2_leakage[iarm][ienergy] == 0 )
            {
              status++;
            }
          else
            {
              h2_leakage[iarm][ienergy]->SetDirectory(0);
            }
          infile->Close();
          delete infile;
        }
    }

  if  (status>0 )
    {
      return 0;
    }

  // Now copy this to the internal array
  int leakindex = 0;
  for (int iarm=0; iarm<2; iarm++)
    {
      for (int ienergy=0; ienergy<NUM_EBINS; ienergy++)
        {
          TH1D *temp_x = h2_leakage[iarm][ienergy]->ProjectionX();
          TH1D *temp_y = h2_leakage[iarm][ienergy]->ProjectionY();
          //int nbinsx = temp_x->GetNbinsX();
          //int nbinsy = temp_y->GetNbinsX();

          //for (int iy=1; iy<=nbinsy; iy++)
          for (int iy=1; iy<=100; iy++)
            {
              float y = temp_y->GetBinCenter(iy);
              uint16_t halfy = f_half_from_float( y );
              //for (int ix=1; ix<=nbinsx; ix++)
              for (int ix=1; ix<=100; ix++)
                {
                  float x = temp_x->GetBinCenter(ix);
                  float temp_leakage = h2_leakage[iarm][ienergy]->GetBinContent(ix,iy);
                  float temp_dleakage = h2_leakage[iarm][ienergy]->GetBinError(ix,iy);
                  pdbleakage[leakindex].setX( f_half_from_float( x ) );
                  pdbleakage[leakindex].setY( halfy );
                  pdbleakage[leakindex].setLeakage( f_half_from_float( temp_leakage ) );
                  pdbleakage[leakindex].setLeakageError( f_half_from_float( temp_dleakage ) );
                  ++leakindex;
                }
            }

          delete temp_x;
          delete temp_y;
        }
    }

  return 1;
}
*/

/*
const float MpcCalib::get_leakage_correction(const int iarm, const float e, const float x, const float y)
{
  recoConsts* rc = recoConsts::instance();
  if(rc->FlagExist("MPC_NO_LEAKAGE")) {
    return 1;
  }
  static const float EBINS[] = { 0., 5., 10., 20. };
  int ebin = 1;
  if ( e < EBINS[1] )      ebin = 0;
  else if ( e < EBINS[2] ) ebin = 1;
  else if ( e < EBINS[3] ) ebin = 2;
  else                     ebin = 3;

  // get leakage correction here
  if ( h2_leakage[iarm][ebin] == 0 )
    {
      //cerr << PHWHERE << " leakage corrections are missing for arm, energy: " << iarm << "\t" << e << endl;
      return 1.0;
    }

  Int_t ibin = h2_leakage[iarm][ebin]->FindBin(fabs(x),fabs(y));
  Float_t leakage_correction = h2_leakage[iarm][ebin]->GetBinContent(ibin);
  if ( leakage_correction <= 0. )
    {
      // this should never happen 
      return 1.0;
    }

  return leakage_correction;
}
*/

int MpcCalib::Download_All(const PHTimeStamp& tstamp)
{
  status = 0;

  status += Download(tstamp,"GAINS");
  status += Download(tstamp,"PED");
  status += Download(tstamp,"LOPOSTPED");
  status += Download(tstamp,"LOPREPED");
  status += Download(tstamp,"HIPOSTPED");
  status += Download(tstamp,"HIPREPED");
  status += Download(tstamp,"LEASTCOUNT");
  status += Download(tstamp,"OVERFLOW");
  status += Download(tstamp,"HILORATIO");
  status += Download(tstamp,"HILOLIMIT");
  status += Download(tstamp,"LED");
  status += Download(tstamp,"PSHAPE");
  status += Download(tstamp,"PSHERR");
  status += Download(tstamp,"NP1SHAPE");
  status += Download(tstamp,"NP1SHERR");
  //status += Download(tstamp,"GAINCORR");
  //status += Download(tstamp,"LEAKAGE");

  return status;
}

int MpcCalib::Download_All(const PHTimeStamp& tstamp, int run_number)
{
  status = 0;
	
  status += Download(tstamp,"GAINS");
  if(run_number >= (int)BEGIN_OF_RUN12) 
    {
      status += Download(tstamp,"PED");
      status += Download(tstamp,"PSHAPE");
      status += Download(tstamp,"PSHERR");
      status += Download(tstamp,"NP1SHAPE");
      status += Download(tstamp,"NP1SHERR");
    }
  if(run_number < (int)BEGIN_OF_RUN12_PP510){
		status += Download(tstamp,"LOPOSTPED");
		status += Download(tstamp,"LOPREPED");
		status += Download(tstamp,"HIPOSTPED");
		status += Download(tstamp,"HIPREPED");
		status += Download(tstamp,"LEASTCOUNT");
		status += Download(tstamp,"OVERFLOW");
		status += Download(tstamp,"HILORATIO");
		status += Download(tstamp,"HILOLIMIT");
	}
  status += Download(tstamp,"LED");
  //status += Download(tstamp,"GAINCORR");
  //status += Download(tstamp,"LEAKAGE");

  return status;
}

int MpcCalib::Download(const PHTimeStamp& tstamp, const string& what)
{
  string classname = "PdbPmtPeakBank";
  string calibname;
  PdbPmtPeak *data = 0;
  PdbMpcGainCorr *gdata = 0;
  PdbMpcLed *led_data = 0;
  PdbMpcShape *pshapes_data = 0;
  PdbMpcShape *psherrs_data = 0;
  PdbMpcShape *np1shapes_data = 0;
  PdbMpcShape *np1sherrs_data = 0;
  //PdbMpcLeakage *ldata = 0;
 
  int bankid = 0; // used only for those calibs where bankid != 0

  if ( what == "GAINS" )
    {
      calibname = "mpcgain";
      data = adc_gain;
    }
  else if ( what == "PED" )
    {
      calibname = "mpcped";
      data = ped;
    }
  else if ( what == "LOPOSTPED" )
    {
      calibname = "mpclopostped";
      data = lgpost;
    }
  else if ( what == "LOPREPED" )
    {
      calibname = "mpclopreped";
      data = lgpre;
    }
  else if ( what == "HIPOSTPED" )
    {
      calibname = "mpchipostped";
      data = hgpost;
    }
  else if ( what == "HIPREPED" )
    {
      calibname = "mpchipreped";
      data = hgpre;
    }
  else if ( what == "LEASTCOUNT" )
    {
      calibname = "mpctdcleastcount";
      data = tdc_leastcount;
    }
  else if ( what == "OVERFLOW" )
    {
      calibname = "mpctdcoverflow";
      data = tdc_oflow;
    }
  else if ( what == "HILORATIO" )
    {
      calibname = "mpchiloratio";
      data = hilo_ratio;
    }
  else if ( what == "HILOLIMIT" )
    {
      calibname = "mpchilolimit";
      data = hilo_limit;
    }
  else if ( what == "LED" )
    {
      classname = "PdbMpcLedBank";
      calibname = "mpcled";
      led_data = led;
      bankid = 4;
    }
  else if ( what == "PSHAPE" )
    {
      classname = "PdbMpcShapeBank";
      calibname = "mpcshape";
      pshapes_data = pshapes;
      bankid = 0; // type is 0 for photons, 1 for NP1, 2 for NP2, etc.
    }
  else if ( what == "PSHERR" )
    {
      classname = "PdbMpcShapeBank";
      calibname = "mpcshape";
      psherrs_data = psherrs;
      bankid = 100; // the errors are 100 + type
    }
  else if ( what == "NP1SHAPE" )
    {
      classname = "PdbMpcShapeBank";
      calibname = "mpcshape";
      np1shapes_data = np1shapes;
      bankid = 1; // type is 0 for photons, 1 for NP1, 2 for NP2, etc.
    }
  else if ( what == "NP1SHERR" )
    {
      classname = "PdbMpcShapeBank";
      calibname = "mpcshape";
      np1sherrs_data = np1sherrs;
      bankid = 101; // the errors are 100 + type
    }
/*
  else if ( what == "GAINCORR" )
    {
      calibname = "mpcgaincorr";
      gdata = gain_corr;
    }
*/
/*
  else if ( what == "LEAKAGE" )
    {
      calibname = "mpcleakage";
      ldata = pdbleakage;
    }
*/
  else
    {
      cout << PHWHERE << " No such database type, " << what << endl;
      return -1;
    }

  // Download from the database
  PdbBankManager *bankManager = PdbBankManager::instance();
  if ( bankManager==0 )
    {
      cout << PHWHERE << " Failed to get bankManager for " << what << endl;
      return -1;
    }

  // Get application manager class.
  PdbApplication *application = bankManager->getApplication();
  if ( application==0 )
    {
      cout << PHWHERE << " Failed to get PdbApplication for " << what << endl;
      return -1;
    }

  if ( application->startRead() )
    {
      PHTimeStamp tSearch = tstamp;
      PdbBankID bankID;
      bankID.setInternalValue(bankid);

      cout << calibname << endl;
      PdbCalBank *mpcBank = bankManager->fetchBank(classname.c_str(),
                                                  bankID,
                                                  calibname.c_str(),
                                                  tSearch);
      if (mpcBank)
        {
          mpcBank->printHeader();
          StartTime = mpcBank->getStartValTime();
          EndTime = mpcBank->getEndValTime();
/*
          if ( calibname == "mpcleakage")
            {
              nleakage = mpcBank->getLength();
            }
*/
          for (unsigned int ich = 0; ich < mpcBank->getLength(); ich++)
            {
              if ( calibname == "mpcgaincorr" ) 
                {
                  *(gdata+ich) = (PdbMpcGainCorr &)mpcBank->getEntry(ich);
                }
              else if ( calibname == "mpcled" ) 
                {
                  //*(led_data+ich) = (PdbMpcLed &)mpcBank->getEntry(ich);
                  PdbMpcLed temp_led = (PdbMpcLed &)mpcBank->getEntry(ich);
                  int fee576 = temp_led.get_fee576ch();
                  *(led_data + fee576) = temp_led;
                  if ( fee576>=0 && fee576<8 )
                    {
                      cout << "Getting MPCLED " << ich << " "
                           << (led_data+fee576)->get_fee576ch() << " "
                           << (led_data+fee576)->get_mean() << endl;
                    }
                }
              else if ( what == "PSHAPE" ) 
                {
                  // First the photon shapes
                  PdbMpcShape temp_shape = (PdbMpcShape &)mpcBank->getEntry(ich);
                  int fee576 = temp_shape.get_fee576ch();
                  *(pshapes_data + fee576) = temp_shape;
                }
              else if ( what == "PSHERR" )
                {
                  // Now the errors
                  PdbMpcShape temp_err = (PdbMpcShape &)mpcBank->getEntry(ich);
                  int fee576 = temp_err.get_fee576ch();
                  *(psherrs_data + fee576) = temp_err;
                }
              else if ( what == "NP1SHAPE" ) 
                {
                  // First the photon shapes
                  PdbMpcShape temp_shape = (PdbMpcShape &)mpcBank->getEntry(ich);
                  int fee576 = temp_shape.get_fee576ch();
                  *(np1shapes_data + fee576) = temp_shape;
                }
              else if ( what == "NP1SHERR" )
                {
                  // Now the errors
                  PdbMpcShape temp_err = (PdbMpcShape &)mpcBank->getEntry(ich);
                  int fee576 = temp_err.get_fee576ch();
                  *(np1sherrs_data + fee576) = temp_err;
                }
/*
              else if ( calibname == "mpcleakage" ) 
                {
                  *(ldata+ich) = (PdbMpcLeakage &)mpcBank->getEntry(ich);
                }
*/
              else
                {
                  *(data+ich) = (PdbPmtPeak &)mpcBank->getEntry(ich);
                }
            }

          delete mpcBank;
        }
      else
        {
          return 1;
        }
      application->commit();
    }
  else
    {
      application->abort();
      cout << PHWHERE << " Transaction aborted." << endl;
      return 1;
    }

/*
  if ( calibname == "mpcleakage")
    {
      // Fill the histograms
      int leakindex = 0;
      for (int iarm=0; iarm<2; iarm++)
        {
          for (int ienergy=0; ienergy<4; ienergy++)
            {
              if ( h2_leakage[iarm][ienergy]==0 )
                {
 cout << "In mpcleakage creation" << endl;
                  TString name = "leakage_mean_offset_arm";
                  name += iarm; name += "_e"; name += ienergy;
                  h2_leakage[iarm][ienergy] = new TH2F(name,name,100,0.,20.,100,0.,20.);
                }
              else
                {
                  h2_leakage[iarm][ienergy]->Reset();
                }

              for (int iy=1; iy<=100; iy++)
                {
                  for (int ix=1; ix<=100; ix++)
                    {
                      float temp_leak = f_half_to_float( pdbleakage[leakindex].getLeakage() );
                      float temp_leakerr = f_half_to_float( pdbleakage[leakindex].getLeakageError() );
                      h2_leakage[iarm][ienergy]->SetBinContent(ix,iy,temp_leak);
                      h2_leakage[iarm][ienergy]->SetBinError(ix,iy,temp_leakerr);
                      ++leakindex;
                    }
                }

            }
        }
    }
*/

  application->DisconnectDB();

  return 0;
}

int MpcCalib::StoreInDatabase(PHTimeStamp& tStart, const string& what, const string& username, const string& description, const PHTimeStamp& tStop)
{
  PdbBankManager *bankManager = PdbBankManager::instance();
  if ( bankManager==0 )
    {
      cout << PHWHERE << " Failed to get bankManager" << endl;
      return -1;
    }
  PdbApplication *application = bankManager->getApplication();
  if ( application==0 )
    {
      cout << PHWHERE << " Failed to get PdbApplication" << endl;
      return -1;
    }

  string classname = "PdbPmtPeakBank";
  string calibname;
  PdbPmtPeak *data = 0;
  PdbMpcGainCorr *gdata = 0;
  PdbMpcLed *ldata = 0;
  PdbMpcShape *pshapes_data = 0;
  PdbMpcShape *psherrs_data = 0;
  PdbMpcShape *np1shapes_data = 0;
  PdbMpcShape *np1sherrs_data = 0;
  //PdbMpcLeakage  *ldata = 0;
 
  int bankid = 0;
  if ( what == "GAINS" )
    {
      calibname = "mpcgain";
      data = adc_gain;
    }
  else if ( what == "PED" )
    {
      calibname = "mpcped";
      data = ped;
    }
  else if ( what == "LOPOSTPED" )
    {
      calibname = "mpclopostped";
      data = lgpost;
    }
  else if ( what == "LOPREPED" )
    {
      calibname = "mpclopreped";
      data = lgpre;
    }
  else if ( what == "HIPOSTPED" )
    {
      calibname = "mpchipostped";
      data = hgpost;
    }
  else if ( what == "HIPREPED" )
    {
      calibname = "mpchipreped";
      data = hgpre;
    }
  else if ( what == "LEASTCOUNT" )
    {
      calibname = "mpctdcleastcount";
      data = tdc_leastcount;
    }
  else if ( what == "OVERFLOW" )
    {
      calibname = "mpctdcoverflow";
      data = tdc_oflow;
    }
  else if ( what == "HILORATIO" )
    {
      calibname = "mpchiloratio";
      data = hilo_ratio;
    }
  else if ( what == "HILOLIMIT" )
    {
      calibname = "mpchilolimit";
      data = hilo_limit;
    }
  else if ( what == "PSHAPE" )
    {
      classname = "PdbMpcShapeBank";
      calibname = "mpcshape";
      pshapes_data = pshapes;
      bankid = 0;
    }
  else if ( what == "PSHERR" )
    {
      classname = "PdbMpcShapeBank";
      calibname = "mpcshape";
      psherrs_data = psherrs;
      bankid = 100;
    }
  else if ( what == "NP1SHAPE" )
    {
      classname = "PdbMpcShapeBank";
      calibname = "mpcshape";
      np1shapes_data = np1shapes;
      bankid = 1;
    }
  else if ( what == "NP1SHERR" )
    {
      classname = "PdbMpcShapeBank";
      calibname = "mpcshape";
      np1sherrs_data = np1sherrs;
      bankid = 101;
    }
  else if ( what == "LED" )
    {
      classname = "PdbMpcLedBank";
      calibname = "mpcled";
      ldata = led;
      bankid = 4;
    }
/*
  else if ( what == "GAINCORR" )
    {
      classname = "PdbMpcGainCorrBank";
      calibname = "mpcgaincorr";
      gdata = gain_corr;
    }
*/
/*
  else if ( what == "LEAKAGE" )
    {
      classname = "PdbMpcLeakageBank";
      calibname = "mpcleakage";
      ldata = pdbleakage;
    }
*/

  cout << "Opening FD in update mode.." << endl;
  if (application->startUpdate())
    {
      //PHTimeStamp tStop = PHTimeStamp(2020, 1, 1, 0, 0, 0);     // canonical end of run time
      PdbBankID bankID;

      PdbCalBank *prevBank = bankManager->fetchBank(classname.c_str(), bankID, calibname.c_str(), tStart);

      if (prevBank)
        {
          cout << " overlapping bank found. Changing the EndValTime of it " << endl;
          //tStop = prevBank->getEndValTime();
          prevBank->setEndValTime(tStart);
          //cout << " and setting current EndValTime to " << tStop << endl;
          delete prevBank;
        }

      // Inputs for calibration bunk header informations.
      PHString Description;
      if ( description.size() == 0 )
        {
          char tempdescription[240];
          cout << "Please input description for this calibration parameter:" << endl;
          cin.getline(tempdescription, 240);
          Description = tempdescription;
        }
      else
        {
          Description = description.c_str();
        }

      PHString UserName;
      if ( username.size() == 0 )
        {
          char tempname[20];
          cout << "Please enter your name:" << endl;
          cin.getline(tempname, 20);
          UserName = tempname;
        }
      else
        {
          UserName = username.c_str();
        }

      bankID.setInternalValue(bankid);

      PdbCalBank *mpcBank = bankManager->createBank(classname.c_str(),
                                                    bankID,
                                                    Description.getString(),
                                                    tStart,
                                                    (PHTimeStamp&)tStop,
                                                    calibname.c_str());

/*
      if ( calibname == "mpcleakage" )
        {
          mpcBank->setLength(80000);
        }
      else
*/
        {
          mpcBank->setLength(576);
        }

      mpcBank->setUserName(UserName);

      for (unsigned int ich = 0; ich < mpcBank->getLength(); ich++)
        {
          if ( calibname == "mpcgaincorr" )
            {
              //cout << "mpcgaincorr " << ich << endl;
              PdbMpcGainCorr *entry = (PdbMpcGainCorr*) &(mpcBank->getEntry(ich));
              *entry = *(gdata+ich);
            }
          else if ( calibname == "mpcled" )
            {
              PdbMpcLed *entry = (PdbMpcLed*) &(mpcBank->getEntry(ich));
              *entry = *(ldata+ich);
              //cout << "mpcledyyy " << entry->get_fee576ch() << "\t" << entry->get_mean() << endl;
            }
          else if ( what == "PSHAPE" )
            {
              PdbMpcShape *entry = (PdbMpcShape*) &(mpcBank->getEntry(ich));
              *entry = *(pshapes_data+ich);
            }
          else if ( what == "PSHERR" )
            {
              PdbMpcShape *entry = (PdbMpcShape*) &(mpcBank->getEntry(ich));
              *entry = *(psherrs_data+ich);
            }
          else if ( what == "NP1SHAPE" )
            {
              PdbMpcShape *entry = (PdbMpcShape*) &(mpcBank->getEntry(ich));
              *entry = *(np1shapes_data+ich);
            }
          else if ( what == "NP1SHERR" )
            {
              PdbMpcShape *entry = (PdbMpcShape*) &(mpcBank->getEntry(ich));
              *entry = *(np1sherrs_data+ich);
            }
/*
          else if ( calibname == "mpcleakage" )
            {
              PdbMpcLeakage *entry = (PdbMpcLeakage*) &(mpcBank->getEntry(ich));
              *entry = *(ldata+ich);
            }
*/
          else
            {
              PdbPmtPeak *entry = (PdbPmtPeak*) &(mpcBank->getEntry(ich));
              *entry = *(data+ich);
            }
        }

      application->commit();
    }
  else
    {
      cout << PHWHERE << "failed to start dbase application for update" << endl;
      return 1;
    }

  application->DisconnectDB();

  return 1;
}

int MpcCalib::Download_DeadHot(const int run)
{
  // get database location
/*
  const char *dbase_dir = getenv("MPC_DATABASE");
  if ( dbase_dir==0 ) return 0;
*/

  const char *dbase_dir = "/phenix/u/chiu/mpc/database/";
  string dbase_location(dbase_dir);

  if ( run<205365 )
    {
      dbase_location += "deadhot_62trans_8014.list";
    }
//  else if ( run>=205365 && run<205441 )
  else if ( run>=205365 )
    {
      dbase_location += "deadhot_62trans_8021.list";
    }
/*
  else
    {
      dbase_location += "deadhot_62trans_8023.list";
    }
*/

  ifstream infile( dbase_location.c_str() );
  if ( !infile.is_open() )
    {
      cout << PHWHERE << "unable to open " << dbase_location << endl;
      status = -3;
      return status;
    }
  else
    {
      cout << "DEADHOT " << dbase_location << endl;
    }

   int fee576ch;
   int value;
   while ( infile >> fee576ch >> value )
     {
       deadhot[fee576ch] = value;
     }

  return 1;
}

void MpcCalib::Reset()
{
  // Set all initial values
  for (int ifee576ch=0; ifee576ch<MAXCH; ifee576ch++)
    {
      adc_gain[ifee576ch].setPeakChannel(0.);
      adc_gain[ifee576ch].setDeviation(0.);
      adc_gain[ifee576ch].setStatus(-1);

      ped[ifee576ch].setPeakChannel(0.);
      ped[ifee576ch].setDeviation(0.);
      ped[ifee576ch].setStatus(-1);

      tdc_oflow[ifee576ch].setPeakChannel(4096.);
      tdc_oflow[ifee576ch].setDeviation(0.);
      tdc_oflow[ifee576ch].setStatus(-1);

      tdc_leastcount[ifee576ch].setPeakChannel(-1.);
      tdc_leastcount[ifee576ch].setDeviation(0.);
      tdc_leastcount[ifee576ch].setStatus(-1);

      hilo_ratio[ifee576ch].setPeakChannel(-1.);
      hilo_ratio[ifee576ch].setDeviation(0.);
      hilo_ratio[ifee576ch].setStatus(-1);

      hilo_limit[ifee576ch].setPeakChannel(0.);
      hilo_limit[ifee576ch].setDeviation(0.);
      hilo_limit[ifee576ch].setStatus(-1);

      gain_corr[ifee576ch].reset();

      slewcor0[ifee576ch] = 0;
      slewcor1[ifee576ch] = 0;

      t0[ifee576ch] = 0;

      deadhot[ifee576ch] = 0;

      pshapes[ifee576ch].reset();
      psherrs[ifee576ch].reset();
      np1shapes[ifee576ch].reset();
      np1sherrs[ifee576ch].reset();

      for (int iamu=0; iamu<MAXAMU; iamu++)
        {
          hgpre[ifee576ch*MAXAMU+iamu].setPeakChannel(0.);
          hgpre[ifee576ch*MAXAMU+iamu].setDeviation(0.);
          hgpre[ifee576ch*MAXAMU+iamu].setStatus(-1);

          hgpost[ifee576ch*MAXAMU+iamu].setPeakChannel(0.);
          hgpost[ifee576ch*MAXAMU+iamu].setDeviation(0.);
          hgpost[ifee576ch*MAXAMU+iamu].setStatus(-1);

          lgpre[ifee576ch*MAXAMU+iamu].setPeakChannel(0.);
          lgpre[ifee576ch*MAXAMU+iamu].setDeviation(0.);
          lgpre[ifee576ch*MAXAMU+iamu].setStatus(-1);

          lgpost[ifee576ch*MAXAMU+iamu].setPeakChannel(0.);
          lgpost[ifee576ch*MAXAMU+iamu].setDeviation(0.);
          lgpost[ifee576ch*MAXAMU+iamu].setStatus(-1);
        }
    }

  // Now set the leakage histograms pointers to 0
  //memset(h2_leakage,0,sizeof(h2_leakage));
}

void MpcCalib::Dump_to_file(const std::string &what)
{
  // make timerange string
  string timerange = ".";
  timerange.append( getStartTime()->formatTimeString() );
  timerange.append("-");
  timerange.append( getEndTime()->formatTimeString() );

  if ( what == "ALL" || what == "GAINS" )
    {
      string full_outfname = "MpcCal.gains";
      full_outfname.append( timerange );
      ofstream outfile(full_outfname.c_str());

      for (int ifeech=0; ifeech<MAXCH; ifeech++)
        {
          outfile << ifeech << "\t"
                  << adc_gain[ifeech].getPeakChannel() << "\t"
                  << adc_gain[ifeech].getDeviation() << "\t"
                  << adc_gain[ifeech].getStatus() << endl;
        }

      outfile.close();
    }

/*
  if ( what == "ALL" || what == "GAINCORR" )
    {
      string full_outfname = "MpcCal.gaincorr";
      full_outfname.append( timerange );
      ofstream outfile(full_outfname.c_str());

      for (int ifeech=0; ifeech<MAXCH; ifeech++)
        {
          outfile << ifeech << "\t"
                  << gain_corr[ifeech].getVersion() << "\t"
                  << gain_corr[ifeech].getPar(0) << "\t"
                  << gain_corr[ifeech].getPar(1) << "\t"
                  << gain_corr[ifeech].getPar(2) << "\t"
                  << gain_corr[ifeech].getPar(3) << "\t"
                  << gain_corr[ifeech].getChi2() << endl;
        }

      outfile.close();
    }
*/

  if ( what == "ALL" || what == "LED" )
    {
      string full_outfname = "MpcCal.led";
      full_outfname.append( timerange );
      ofstream outfile(full_outfname.c_str());

      for (int ifeech=0; ifeech<MAXCH; ifeech++)
        {
          if ( led[ifeech].get_status() < -999 ) continue;	// empty channel
          outfile << led[ifeech].get_status() << "\t"
                  << led[ifeech].get_fee576ch() << "\t"
                  << led[ifeech].get_mean() << "\t"
                  << led[ifeech].get_dmean() << "\t"
                  << led[ifeech].get_chisquare() << "\t"
                  << led[ifeech].get_ndf() << endl;
        }

      outfile.close();
    }

  if ( what == "ALL" || what == "PSHAPE" )
    {
      // Write out the pshapes
      string full_outfname = "MpcCal.pshape";
      full_outfname.append( timerange );
      ofstream outfile(full_outfname.c_str());

      for (int ifeech=0; ifeech<MAXCH; ifeech++)
        {
          if ( pshapes[ifeech].get_nsamples() <= 0 ) continue;	// empty channel
          outfile << pshapes[ifeech].get_fee576ch() << "\t"
                  << pshapes[ifeech].get_nsamples() << "\t"
                  << pshapes[ifeech].get_start_time() << "\t"
                  << pshapes[ifeech].get_end_time() << endl;
          for (int isamp=0; isamp<pshapes[ifeech].get_nsamples(); isamp++)
            {
              outfile << pshapes[ifeech].getValue(isamp);
              if ( isamp%10 == 9 ) outfile << endl;
              else                 outfile << " ";
            }
          outfile << endl;
        }

      outfile.close();

      // Write out the pshape errors
      full_outfname = "MpcCal.psherr";
      full_outfname.append( timerange );
      ofstream outfile2(full_outfname.c_str());

      for (int ifeech=0; ifeech<MAXCH; ifeech++)
        {
          if ( psherrs[ifeech].get_nsamples() <= 0 ) continue;	// empty channel
          outfile2 << psherrs[ifeech].get_fee576ch() << "\t"
                  << psherrs[ifeech].get_nsamples() << "\t"
                  << psherrs[ifeech].get_start_time() << "\t"
                  << psherrs[ifeech].get_end_time() << endl;
          for (int isamp=0; isamp<psherrs[ifeech].get_nsamples(); isamp++)
            {
              outfile2 << psherrs[ifeech].getValue(isamp);
              if ( (isamp%10) == 9 ) outfile2 << endl;
              else                   outfile2 << " ";
            }
          outfile2 << endl;
        }

      outfile2.close();
    }

  if ( what == "ALL" || what == "NP1SHAPE" )
    {
      // Write out the np1shapes
      string full_outfname = "MpcCal.np1shape";
      full_outfname.append( timerange );
      ofstream outfile(full_outfname.c_str());

      for (int ifeech=0; ifeech<MAXCH; ifeech++)
        {
          if ( np1shapes[ifeech].get_nsamples() <= 0 ) continue;	// empty channel
          outfile << np1shapes[ifeech].get_fee576ch() << "\t"
                  << np1shapes[ifeech].get_nsamples() << "\t"
                  << np1shapes[ifeech].get_start_time() << "\t"
                  << np1shapes[ifeech].get_end_time() << endl;
          for (int isamp=0; isamp<np1shapes[ifeech].get_nsamples(); isamp++)
            {
              outfile << np1shapes[ifeech].getValue(isamp);
              if ( isamp%10 == 9 ) outfile << endl;
              else                 outfile << " ";
            }
          outfile << endl;
        }

      outfile.close();

      // Write out the np1shapes errors
      full_outfname = "MpcCal.np1sherr";
      full_outfname.append( timerange );
      ofstream outfile2(full_outfname.c_str());

      for (int ifeech=0; ifeech<MAXCH; ifeech++)
        {
          if ( np1sherrs[ifeech].get_nsamples() <= 0 ) continue;	// empty channel
          outfile2 << np1sherrs[ifeech].get_fee576ch() << "\t"
                  << np1sherrs[ifeech].get_nsamples() << "\t"
                  << np1sherrs[ifeech].get_start_time() << "\t"
                  << np1sherrs[ifeech].get_end_time() << endl;
          for (int isamp=0; isamp<np1sherrs[ifeech].get_nsamples(); isamp++)
            {
              outfile2 << np1sherrs[ifeech].getValue(isamp);
              if ( (isamp%10) == 9 ) outfile2 << endl;
              else                   outfile2 << " ";
            }
          outfile2 << endl;
        }

      outfile2.close();
    }

  if ( what == "ALL" || what == "OVERFLOW" )
    {
      string full_outfname = "MpcCal.overflow";
      full_outfname.append( timerange );
      ofstream outfile(full_outfname.c_str());

      for (int ifeech=0; ifeech<MAXCH; ifeech++)
        {
          outfile << ifeech << "\t"
                  << tdc_oflow[ifeech].getPeakChannel() << "\t"
                  << tdc_oflow[ifeech].getDeviation() << "\t"
                  << tdc_oflow[ifeech].getStatus() << endl;
        }
      outfile.close();
    }

  if ( what == "ALL" || what == "LEASTCOUNT" )
    {
      string full_outfname = "MpcCal.leastcount";
      full_outfname.append( timerange );
      ofstream outfile(full_outfname.c_str());

      for (int ifeech=0; ifeech<MAXCH; ifeech++)
        {
          outfile << ifeech << "\t"
                  << tdc_leastcount[ifeech].getPeakChannel() << "\t"
                  << tdc_leastcount[ifeech].getDeviation() << "\t"
                  << tdc_leastcount[ifeech].getStatus() << endl;
        }
      outfile.close();
    }

  if ( what == "ALL" || what == "HILORATIO" )
    {
      string full_outfname = "MpcCal.hiloratio";
      full_outfname.append( timerange );
      ofstream outfile(full_outfname.c_str());

      for (int ifeech=0; ifeech<MAXCH; ifeech++)
        {
          outfile << ifeech << "\t"
                  << hilo_ratio[ifeech].getPeakChannel() << "\t"
                  << hilo_ratio[ifeech].getDeviation() << "\t"
                  << hilo_ratio[ifeech].getStatus() << endl;
        }
      outfile.close();
    }

  if ( what == "ALL" || what == "HILOLIMIT" )
    {
      string full_outfname = "MpcCal.hilolimit";
      full_outfname.append( timerange );
      ofstream outfile(full_outfname.c_str());

      for (int ifeech=0; ifeech<MAXCH; ifeech++)
        {
          outfile << ifeech << "\t"
                  << hilo_limit[ifeech].getPeakChannel() << "\t"
                  << hilo_limit[ifeech].getDeviation() << "\t"
                  << hilo_limit[ifeech].getStatus() << endl;
        }
      outfile.close();
    }

  if ( what == "ALL" || what == "PED" )
    {
      string full_outfname = "MpcCal.ped";
      full_outfname.append( timerange );
      ofstream outfile(full_outfname.c_str());

      for (int ifeech=0; ifeech<MAXCH; ifeech++)
        {
          outfile << ifeech << "\t"
                  << ped[ifeech].getPeakChannel() << "\t"
                  << ped[ifeech].getDeviation() << "\t"
                  << ped[ifeech].getStatus() << endl;
        }
      outfile.close();
    }

  if ( what == "ALL" || what == "LOPREPED" )
    {
      string full_outfname = "MpcCal.lopreped";
      full_outfname.append( timerange );
      ofstream outfile(full_outfname.c_str());

      for (int ifeech=0; ifeech<MAXCH; ifeech++)
        {
          for (int iamu=0; iamu<MAXAMU; iamu++)
            {
               outfile << ifeech << "\t" << iamu << "\t"
                       << lgpre[ifeech*MAXAMU+iamu].getPeakChannel() << "\t"
                       << lgpre[ifeech*MAXAMU+iamu].getDeviation() << "\t"
                       << lgpre[ifeech*MAXAMU+iamu].getStatus() << endl;
            }
        }
      outfile.close();
    }

  if ( what == "ALL" || what == "LOPOSTPED" )
    {
      string full_outfname = "MpcCal.lopostped";
      full_outfname.append( timerange );
      ofstream outfile(full_outfname.c_str());

      for (int ifeech=0; ifeech<MAXCH; ifeech++)
        {
          for (int iamu=0; iamu<MAXAMU; iamu++)
            {
               outfile << ifeech << "\t" << iamu << "\t"
                       << lgpost[ifeech*MAXAMU+iamu].getPeakChannel() << "\t"
                       << lgpost[ifeech*MAXAMU+iamu].getDeviation() << "\t"
                       << lgpost[ifeech*MAXAMU+iamu].getStatus() << endl;
            }
        }
      outfile.close();
    }

  if ( what == "ALL" || what == "HIPREPED" )
    {
      string full_outfname = "MpcCal.hipreped";
      full_outfname.append( timerange );
      ofstream outfile(full_outfname.c_str());

      for (int ifeech=0; ifeech<MAXCH; ifeech++)
        {
          for (int iamu=0; iamu<MAXAMU; iamu++)
            {
               outfile << ifeech << "\t" << iamu << "\t"
                       << hgpre[ifeech*MAXAMU+iamu].getPeakChannel() << "\t"
                       << hgpre[ifeech*MAXAMU+iamu].getDeviation() << "\t"
                       << hgpre[ifeech*MAXAMU+iamu].getStatus() << endl;
            }
        }
      outfile.close();
    }

  if ( what == "ALL" || what == "HIPOSTPED" )
    {
      string full_outfname = "MpcCal.hipostped";
      full_outfname.append( timerange );
      ofstream outfile(full_outfname.c_str());

      for (int ifeech=0; ifeech<MAXCH; ifeech++)
        {
          for (int iamu=0; iamu<MAXAMU; iamu++)
            {
               outfile << ifeech << "\t" << iamu << "\t"
                       << hgpost[ifeech*MAXAMU+iamu].getPeakChannel() << "\t"
                       << hgpost[ifeech*MAXAMU+iamu].getDeviation() << "\t"
                       << hgpost[ifeech*MAXAMU+iamu].getStatus() << endl;
            }
        }
      outfile.close();
    }

}

void MpcCalib::Print(Option_t *option) const
{
  std::string what(option);
  if ( what == "ALL" || what == "GAINS" )
    {
      cout << "GAINS " << endl;
      for (int ifeech=0; ifeech<MAXCH; ifeech++)
        {
          cout << ifeech << "\t" << adc_gain[ifeech].getPeakChannel() << endl;
        }
    }

/*
  if ( what == "ALL" || what == "GAINCORR" )
    {
      cout << "GAINCORR " << endl;
      cout << "Version\t" << gain_corr[0].getVersion() << endl;
      for (int ifeech=0; ifeech<MAXCH; ifeech++)
        {
          cout << ifeech << "\t" << gain_corr[ifeech].getPar(0)
               << "\t" << gain_corr[ifeech].getPar(1)
               << "\t" << gain_corr[ifeech].getPar(2)
               << "\t" << gain_corr[ifeech].getPar(3)
               << "\t" << get_gaincorr(ifeech) << endl;
        }
    }
*/

  if ( what == "ALL" || what == "LED" )
    {
      cout << "LED " << endl;
      for (int ifeech=0; ifeech<MAXCH; ifeech++)
        {
          cout << ifeech << "\t" << led[ifeech].get_mean() << endl;
        }
    }

  if ( what == "ALL" || what == "OVERFLOW" )
    {
      cout << "TDC OVERFLOW" << endl;
      for (int ifeech=0; ifeech<MAXCH; ifeech++)
        {
          cout << ifeech << "\t" << tdc_oflow[ifeech].getPeakChannel()
                         << "\t" << tdc_oflow[ifeech].getDeviation() << endl;
        }
    }

  if ( what == "ALL" || what == "LEASTCOUNT" )
    {
      cout << "TDC LEAST COUNT" << endl;
      for (int ifeech=0; ifeech<MAXCH; ifeech++)
        {
          cout << ifeech << "\t" << tdc_leastcount[ifeech].getPeakChannel()
                         << "\t" << tdc_leastcount[ifeech].getDeviation() << endl;
        }
    }

  if ( what == "ALL" || what == "HILO" )
    {
      cout << "HI/LO Ratio and Limit" << endl;
      for (int ifeech=0; ifeech<MAXCH; ifeech++)
        {
          cout << ifeech << "\t" << hilo_ratio[ifeech].getPeakChannel()
                         << "\t" << hilo_limit[ifeech].getPeakChannel() << endl;
        }
    }

  if ( what == "ALL" || what == "DEADHOT" )
    {
      cout << "DEAD/HOT TOWERS" << endl;
      for (int ifeech=0; ifeech<MAXCH; ifeech++)
        {
          if ( deadhot[ifeech]!=0 )
            {
              cout << ifeech << "\t" << deadhot[ifeech] << endl;
            }
        }
    }

  if ( what == "ALL" || what == "PEDESTALS" )
    {
      cout << "New Pedestals" << endl;
      for (int ifeech=0; ifeech<MAXCH; ifeech++)
        {
          cout << ifeech << ":\t"
               << ped[ifeech].getPeakChannel()
               << endl;
        }
    }

  if ( what == "ALL" || what == "PEDESTALS" )
    {
      cout << "HG Pre Pedestal" << endl;
      for (int ifeech=0; ifeech<MAXCH; ifeech++)
        {
          cout << ifeech << ":\t";
          for (int iamu=0; iamu<MAXAMU; iamu++)
            {
              cout << hgpre[ifeech*MAXAMU+iamu].getPeakChannel() << " ";
            }
          cout << endl;
        }
      cout << "HG Post Pedestal" << endl;
      for (int ifeech=0; ifeech<MAXCH; ifeech++)
        {
          cout << ifeech << ":\t";
          for (int iamu=0; iamu<MAXAMU; iamu++)
            {
              cout << hgpost[ifeech*MAXAMU+iamu].getPeakChannel() << " ";
            }
          cout << endl;
        }
      cout << "LG Pre Pedestal" << endl;
      for (int ifeech=0; ifeech<MAXCH; ifeech++)
        {
          cout << ifeech << ":\t";
          for (int iamu=0; iamu<MAXAMU; iamu++)
            {
              cout << lgpre[ifeech*MAXAMU+iamu].getPeakChannel() << " ";
            }
          cout << endl;
        }
      cout << "LG Post Pedestal" << endl;
      for (int ifeech=0; ifeech<MAXCH; ifeech++)
        {
          cout << ifeech << ":\t";
          for (int iamu=0; iamu<MAXAMU; iamu++)
            {
              cout << lgpost[ifeech*MAXAMU+iamu].getPeakChannel() << " ";
            }
          cout << endl;
        }
    }
}

float MpcCalib::get_gaincorr(const int ich) const
{
  recoConsts *rc = recoConsts::instance();
  if ( rc->FlagExist("RUNNUMBER") )
    {
      // Get Time for this run
      int run_number = rc->get_IntFlag("RUNNUMBER");
      RunToTime *runtotime = RunToTime::instance();
      PHTimeStamp *begintime = runtotime->getBeginTime( run_number );
      PHTimeStamp *endtime = runtotime->getEndTime( run_number );
      time_t meantime = begintime->getTics()/2 + endtime->getTics()/2;
      delete begintime;
      delete endtime;

      float gc = gain_corr[ich].getGainCorr( meantime );
      return gc;
    }

  return 1.0;	// defaults to 1 if runnumber doesn't exist. maybe we should be noisy about this?
}

void MpcCalib::AddToNodeTree(PHCompositeNode *topNode)
{
  PHCompositeNode *mpcNode;
  PHNodeIterator iter(topNode);
  mpcNode = dynamic_cast<PHCompositeNode*>(iter.findFirst("PHCompositeNode", "MPC"));
  if (!mpcNode) // create mpcNode
    {
      mpcNode = new PHCompositeNode("MPC");
      topNode->addNode(mpcNode);
    }
  //PHObjectNode_t *MpcCalibNode = new PHObjectNode_t(__instance, "MpcCalib", "PHObject");
  PHObjectNode_t *MpcCalibNode = new PHObjectNode_t(this, "MpcCalib", "PHObject");
  mpcNode->addNode(MpcCalibNode);
}

int MpcCalib::IsValid(const int verbosity) const
{
  if ( verbosity>0 && status<=0 )
    {
      if ( status == -1 )
        {
          cout << PHWHERE << "Please set environment variable MPC_DATABASE" << endl;
        }
      else if ( status == -2 )
        {
          cout << PHWHERE << "Database not yet implemented" << endl;
        }
      else
        {
          cout << PHWHERE << "Unknown status " << status << endl;
        }
    }

  return status;
}

