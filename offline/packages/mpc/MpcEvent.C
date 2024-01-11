#include <unistd.h>
#include <cassert>
#include <string>
#include <iostream>
#include <fstream>
#include <map>
#include <algorithm>

#include <MpcEvent.h>
#include <packet_hbd_fpgashort.h>

#include <PHTimeStamp.h>
#include <RunToTime.hh>

#include <PHCompositeNode.h>
#include <PHGlobal.h>
#include <PHTimeStamp.h>
#include <Event.h>
#include <getClass.h>
#include <recoConsts.h>
#include <VtxOut.h>
#include <BbcOut.h>
#include <fkinWrapper.h>
#include <GeaTrkStack.h>

#include <mpcSampleContainer.h>
#include <mpcSampleV1.h>
#include <mpcRawContainer.h>
#include <mpcRawContentV2.h>
#include <mpcRawContentV3.h>
#include <mpcRawContentV4.h>
#include <mpcRawContentV5.h>
#include <mpcTowerContainer.h>
#include <mpcTowerContentV1.h>
#include <mpcSimTowerContentV1.h>
#include <mpcClusterContainerV1.h>
#include <mpcClusterContentV1.h>
#include <mpcClusterContainerV2.h>
#include <mpcClusterContentV2.h>
#include <mpcGeaTowerContainerV1.h>
#include <mpcGeaTowerContentV1.h>

#include <MpcPISAHit.h>

#include <MpcCluster.h>

#include <MpcMap.h>
#include <MpcCalib.h>
#include <PdbMpcShape.hh>

#include <MpcSectorRec.h>
#include <MpcSectorRecV2.h>

#include <TTree.h>
#include <TFile.h>
#include <TGraph.h>
#include <TGraphErrors.h>
#include <TCanvas.h>
#include <TMath.h>
#include <TPad.h>
#include <TStyle.h>
#include <TMinuit.h>
#include <TF1.h>
#include <TRandom.h>

//#include "gsl/gsl_const.h"
//static const float C = GSL_CONST_CGS_SPEED_OF_LIGHT / 1e9;

using namespace std;

MpcEvent::MpcEvent(PHCompositeNode *topNode)
{
  mpcraw = 0;
  mpcraw2 = 0;
  mpcsamples = 0;
  mpctower = 0;
  mpcclus = 0;
  phglobal = 0;
  event = 0;
  fit_shape = 0;
  fit_sherr = 0;
  first_time = 0;

  RunEnergyCorr = 1.0;

  // These variables control the behavior of the reconstruction
  reco_consts = recoConsts::instance();

  mpc_reco_mode = reco_consts->get_IntFlag("MPC_RECO_MODE", 0x7);
  cout << "MPC_RECO_MODE\t" << mpc_reco_mode << endl;

  mpc_event_display = reco_consts->get_IntFlag("MPC_EVENT_DISPLAY", 0);
  mpc_verbosity = reco_consts->get_IntFlag("MPC_VERBOSITY", 0);
  simulationflag = 0;
  if (reco_consts->FlagExist("SIMULATIONFLAG"))
    {
      simulationflag = reco_consts->get_IntFlag("SIMULATIONFLAG");
    }
  mpc_cluster_alg = reco_consts->get_IntFlag("MPC_CLUSTER_ALG", 1);
  MpcCluster::SetMomentType(reco_consts->get_IntFlag("MPC_CG_FLAG", 2) );
  MpcSectorRec::SetMomentType(reco_consts->get_IntFlag("MPC_CG_FLAG", 2) );
  
  s_cluster_position = 0;

  // Gain corrections
  mpc_gaincorr_flag = reco_consts->get_IntFlag("MPC_GAINCORR",0);

  // NEW GAIN FILE
  if ( reco_consts->FlagExist("MPC_APPLYGAIN") )
    {
      mpc_newgain_flag = 1;
      mpc_newgain_file = reco_consts->get_CharFlag("MPC_APPLYGAIN");
      ReadNewGains( mpc_newgain_file.c_str() );
    }
  else
    {
      mpc_newgain_flag = 0;
    }

  // NEW NOISE FILE
  if ( reco_consts->FlagExist("MPC_APPLYNOISE") )
    {
      mpc_newnoise_flag = 1;
      mpc_newnoise_file = reco_consts->get_CharFlag("MPC_APPLYNOISE");
      ReadNewNoise( mpc_newnoise_file.c_str() );
    }
  else
    {
      mpc_newnoise_flag = 0;
    }

  // Whether to reconstruct clusters if there is no good zvertex
  mpc_recobadvtx = -9999.;
  if ( reco_consts->FlagExist("MPC_RECOBADVTX") )
    {
      mpc_recobadvtx = reco_consts->get_FloatFlag("MPC_RECOBADVTX");
    }

  if ( topNode )
    {
      mpcmap = findNode::getClass<MpcMap>(topNode, "MpcMap");
      mpccalib = findNode::getClass<MpcCalib>(topNode, "MpcCalib");
    }
  else
    {
      mpcmap = MpcMap::instance();		// position and grid map
      mpccalib = MpcCalib::instance();		// calibrations class
    }
  if ( (mpc_reco_mode&0x2) == 0x2 )
    {
      GetShapes();			// get waveform shapes from mpccalib
    }

  // arm 0 (S) or arm 1 (N)
  if ( mpc_cluster_alg == 0 )
    {
      fMpcSector[0] = new MpcSectorRec(0);
      fMpcSector[1] = new MpcSectorRec(1);
    }
  else if ( mpc_cluster_alg == 1 )
    {
      fMpcSector[0] = new MpcSectorRecV2(0);
      fMpcSector[1] = new MpcSectorRecV2(1);
    }
  else
    {
      memset(fMpcSector,0,sizeof(fMpcSector));
    }

  // random number generator initialization
  gsl_rng_env_setup();
  gsl_rand_type = gsl_rng_default;
  gsl_rand_gen = gsl_rng_alloc( gsl_rand_type );

  // DEBUG

  NewEvent = false; 

  //to get proper index of how to draw the (ix,iy) on a canvas
  int index = 1;
  for(int j = 17; j >= 0; j--){
   for(int i = 0; i <= 17; i++){
      canIndex[i][j] = index;
      index++;
    }//end i(ix)
  }//end j (iy)

}

MpcEvent::~MpcEvent()
{
  if ( fMpcSector[0]!=0 ) delete fMpcSector[0];
  if ( fMpcSector[1]!=0 ) delete fMpcSector[1];

  gsl_rng_free( gsl_rand_gen );

}

void MpcEvent::Clear()
{
  if ( mpcraw!=0 ) mpcraw->Reset();
  if ( mpcraw2!=0 ) mpcraw2->Reset();	// new electronics
  if ( mpcsamples!=0 ) mpcsamples->Reset();
  if ( mpctower!=0 ) mpctower->Reset();
  if ( mpcclus!=0 ) mpcclus->Reset();
  if ( event!=0 ) event = 0;
}

void MpcEvent::Save()
{
  // Stuff to do at end of run
}

int MpcEvent::ReadNewGains(const char *newgainfile)
{
  // Reset the new gains
  for (int ich=0; ich<NFEECH; ich++)
    {
      mpc_new_gain[ich] = 1.0;
    }

  int ngoodlines = 0;
  int feech;
  int ixpos, iypos;
  float temp_newgain;
  ifstream infile(newgainfile);
  while ( infile >> feech >> ixpos >> iypos >> temp_newgain )
    {
      if ( feech<0 || feech>=NFEECH )
        {
          cout << "MpcEvent::ReadNewGain, ERROR, feech " << feech << " out of valid range" << endl;
          continue;
        }

      mpc_new_gain[feech] = temp_newgain;
      ngoodlines++;
    }
  infile.close();

  return ngoodlines;
}

int MpcEvent::ApplyNewGains(PHCompositeNode *topNode)
{
  mpcTowerContainer *mpctow = findNode::getClass<mpcTowerContainer>(topNode,"mpcTowerContainer");
  if (!mpctow)
    {
      cout << PHWHERE << "Unable to get mpcTowerContainer, is Node missing?" << endl;
      return 0;
    }

  int ntow = mpctow->size();
  for (int itow=0; itow<ntow; itow++)
    {
      mpcTowerContent *tower = mpctow->getTower( itow );
      int feech = tower->get_ch();
      if ( mpcmap->isCrystal( feech ) == 0  ) continue;
      float e = tower->get_energy(0);
      tower->set_energy( e*mpc_new_gain[feech] );
    }

  return 1;
}

int MpcEvent::ReadNewNoise(const char *newnoisefile)
{
  // Reset the new noise
  for (int ich=0; ich<NFEECH; ich++)
    {
      mpc_new_noise[ich] = 0.0;
    }

  int ngoodlines = 0;
  int feech;
  int ixpos, iypos;
  float temp_newnoise;
  ifstream infile(newnoisefile);
  while ( infile >> feech >> ixpos >> iypos >> temp_newnoise )
    {
      if ( feech<0 || feech>=NFEECH )
        {
          cout << "MpcEvent::ReadNewNoise, ERROR, feech " << feech << " out of valid range" << endl;
          continue;
        }

      mpc_new_noise[feech] = temp_newnoise;
      ngoodlines++;
    }
  infile.close();

  return ngoodlines;
}

int MpcEvent::ApplyNewNoise(PHCompositeNode *topNode)
{
  mpcTowerContainer *mpctow = findNode::getClass<mpcTowerContainer>(topNode,"mpcTowerContainer");
  if (!mpctow)
    {
      cout << PHWHERE << "Unable to get mpcTowerContainer, is Node missing?" << endl;
      return 0;
    }

  int ntow = mpctow->size();
  for (int itow=0; itow<ntow; itow++)
    {
      mpcTowerContent *tower = mpctow->getTower( itow );
      int feech = tower->get_ch();
      if ( mpcmap->isCrystal( feech ) == 0  ) continue;
      float e = tower->get_energy(0);
      e += gsl_ran_gaussian( gsl_rand_gen, mpc_new_noise[feech] );
      tower->set_energy( e );
    }

  return 1;
}

PHBoolean MpcEvent::SetRawData(Event *)
{
  return True;
}

PHBoolean MpcEvent::ProcessOneEvent( PHCompositeNode *topNode )
{
  
  // Flag for first call through for each event, 
  // used in algo2
  NewEvent = true; 

  // Now go through and run the various reco modules
  PHBoolean status = False;

  // unpack prdf into MPC
  if ( (mpc_reco_mode&0x1)!=0 )
    {
      status = GetDCM( topNode );
      if ( status==False ) return False;
    }

  // calibrate real mpcRawContainer into mpcTowerContainer
  if ( (mpc_reco_mode&0x2)!=0 && simulationflag==0 )
    {
     
      if ( first_time == 0 )
        {
          RunNumber = reco_consts->get_IntFlag("RUNNUMBER");
	  RunToTime *runtotime = RunToTime::instance();
          PHTimeStamp *begin_timestamp = runtotime->getBeginTime( RunNumber );
          PHTimeStamp *end_timestamp = runtotime->getEndTime( RunNumber );
      
          time_t begintics = begin_timestamp->getTics();
          time_t endtics = end_timestamp->getTics();
          RunMeanTime = endtics/2UL + begintics/2UL;
      
          delete begin_timestamp;
          delete end_timestamp;
          first_time = 1;

          if ( RunNumber>=258249 && RunNumber<=258655 ) 
            {
              double t = double( RunMeanTime-1203121062UL );
              double val = -1.631861e+01
                           + 6.919493e-05*t
                           - 1.164252e-10*t*t
                           + 9.793676e-17*t*t*t
                           - 4.117557e-23*pow(t,4.0)
                           + 6.919303e-30*pow(t,5.0);

              RunEnergyCorr = float( 0.1324/val );
              cout << PHWHERE << " Using Energy Correction " << RunEnergyCorr
                   << " for 258249-258655 Run Period." << endl;
            }
          else if ( RunNumber>=259365 && RunNumber<=259575 ) 
            {
              double t = double( RunMeanTime-1203121062UL );
              double val = 4.740071e+01
                           - 1.056509e-04*t
                           + 9.055678e-11*t*t
                           - 3.627716e-17*t*t*t
                           + 6.408910e-24*pow(t,4.0)
                           - 3.287009e-31*pow(t,5.0);
              RunEnergyCorr = float( 0.1324/val );
              cout << PHWHERE << " Using Energy Correction " << RunEnergyCorr
                   << " for 259365-258575 Run Period." << endl;
            }
        }

      // In the earlier period, we sometimes wrote out mpcsamples
      // but didn't fill mpcraw2, so we fill mpcraw2 in such cases.
      if ( !mpcraw2 ) mpcraw2 = findNode::getClass<mpcRawContainer>(topNode,"MpcRaw2");
      if ( !mpcsamples ) mpcsamples = findNode::getClass<mpcSampleContainer>(topNode,"mpcSampleContainer");
      if ( mpcsamples!=0 && mpcraw2!=0 )
        {
          if ( mpcsamples->size()>0 && mpcraw2->size()==0 )
            {
              GetAmplitudes( topNode );
            }
        }

      status = CalibrateTowers( topNode );
      if ( status==False ) return False;
    }

  // calibrate MpcPISAHits into mpcTowerContainer
  if ( simulationflag==2 )
    {
      status = CalibrateSimTowers( topNode );

      status = True;
    }

  // Apply user specified gains/noise when creating towers or clusters
  if ( (mpc_reco_mode&0x6)!=0 )
    {
      if ( mpc_newgain_flag != 0 )
        {
          ApplyNewGains( topNode );
        }

      if ( mpc_newnoise_flag != 0 )
        {
          ApplyNewNoise( topNode );
        }
    }

  // cluster mpcTowerContainer into mpcClusterContainer
  if ( (mpc_reco_mode&0x4)!=0 )
    {
      // 

      // Now Run Clustering
      DoClustering( topNode );

      status = True;
    }
  
  return True;
}

// Copy the mpc prdf data to mpcRawContent
PHBoolean MpcEvent::GetDCM(PHCompositeNode* topNode)
{
  event = findNode::getClass<Event>(topNode,"PRDF");
  //cout << "event addr " << (unsigned int)event << endl;
  if ( event==0 )
    {
      cout << PHWHERE << "Unable to get PRDF, is Node missing?" << endl;
      return False;
    }

  if ( !mpcraw )
    {
      mpcraw = findNode::getClass<mpcRawContainer>(topNode,"MpcRaw");

      static int ncounts = 0;
      if (!mpcraw && ncounts<10 )
        {
          cout << PHWHERE << "Unable to get MpcRaw, is Node missing?" << endl;
          ncounts++;
          return False;
        }
    }

  if ( !mpcsamples )
    {
      mpcsamples = findNode::getClass<mpcSampleContainer>(topNode,"mpcSampleContainer");
      static int ncounts = 0;
      if (!mpcsamples && ncounts<10 )
        {
          cout << PHWHERE << "Unable to get mpcSampleContainer, is Node missing?" << endl;
          ncounts++;
          return False;
        }
    }

  EventNumber = event->getEvtSequence();
  if (mpc_verbosity>0) 
    {
      cout << "MpcEvent:: evt " << EventNumber << endl;
    }
  // Get the relevant packets from the Event object (PRDF) and
  // copy them into a mpcRawContainer
  static const int NFEM = 18;
  const int MpcPacketId[NFEM] = {	// PacketIDs
    21101, 21102, 21103, 21104, 21105, 21106,			// New Electronics
    21011, 21012, 21021, 21022, 21031, 21032, 21041, 21042,	// Split Readout
    21001, 21002, 21003, 21004 };				// Orig Readout

  mpcRawContentV2 temp_rawcontent;
  mpcSampleV1     temp_sample;

  for (int ifem=0; ifem<NFEM; ifem++)
    {
      Packet *p = event->getPacket( MpcPacketId[ifem] );
      if (p==0) continue;

      if ( ifem<6 )	// Process new mpc electronics
        {
          const int NCHAN_PER_MOD = 48;
          Packet_hbd_fpgashort *hbd_p = dynamic_cast<Packet_hbd_fpgashort*>( p );
          hbd_p->setNumSamples( NSAMPLES );
        
          const int nmod_per_fem = p->iValue(0,"NRMODULES");
          //cout << "nmod_per_fem " << nmod_per_fem << endl;
        
          for (int imod=0; imod<nmod_per_fem; imod++)
            {
              for (int j=0; j<NCHAN_PER_MOD; j++)
                {
                  int fee576ch = ifem*nmod_per_fem*NCHAN_PER_MOD + imod*NCHAN_PER_MOD + j;
                  int ch = imod*NCHAN_PER_MOD + j;
                  temp_sample.set_ch( fee576ch );
                  for (int isample=0; isample<NSAMPLES; isample++)
                    {
                      Short_t val = p->iValue(ch,isample);
        
                      //cout << fee576ch << setw(6) << isample
                      //     << setw(6) << val << setw(6) << hex << val << dec << endl;
             
                      Short_t adc = (val&0x0fff);
                      if ( adc==0 ) continue;
             
                      temp_sample.set_adc(adc);
                      temp_sample.set_sample(isample);
             
                      if ( mpcsamples ) mpcsamples->AddSample( temp_sample );
                    }
                }
            }
        }
      else		// Process Old Electronics
        {
          int tdc_amu  = p->iValue(0,"AMU");	// TDC
          int pre_amu  = p->iValue(1,"AMU");	// PRE SAMPLE
          int post_amu = p->iValue(2,"AMU");	// POST SAMPLE
          int fee576_ch = -1;
          mpcraw->set_amu(tdc_amu,pre_amu,post_amu);
          if (ifem < 14) ifemchmax=72;		// split fibers
          else if (ifem >=14 && ifem < NFEM) ifemchmax=144;	// orig readout
          
          for (int ifemch=0; ifemch<ifemchmax; ifemch++)
            {
              if (ifemchmax==72)
                {
                  fee576_ch = (ifem-6)*ifemchmax + ifemch;
                }
    	      else if (ifemchmax==144)
                {
    	          fee576_ch = (ifem-14)*ifemchmax + ifemch;
                }
    	      if (fee576_ch==-1)continue;

              // we should probably do some quality checks here...
              //dcmbuf[index++] = pa->iValue(0, "ID");	// det id.
              //dcmbuf[index++] = pa->iValue(0, "EVTNR");	// event no.
              //dcmbuf[index++] = pa->iValue(0, "MODULE");	// module no.
              //dcmbuf[index++] = pa->iValue(0, "FLAG");	// flag word
              //dcmbuf[index++] = pa->iValue(0, "BCLK");	// beam clock
              //dcmbuf[index++] = pa->iValue(0, "BOARD");	// board id
              //dcmbuf[index++] = pa->iValue(0, "PARITY");	// parity word
              //dcmbuf[index++] = pa->iValue(0, "SUMMARY");	// summary word
    
              int tdc    = p->iValue(ifemch, 0);
              int lopre  = p->iValue(ifemch, 4);
              int lopost = p->iValue(ifemch, 2);
              int hipost = p->iValue(ifemch, 1);
              int hipre  = p->iValue(ifemch, 3);
    
              if ( tdc!=0 || lopost!=0 || lopre!=0 || hipost!=0 || hipre!=0 )
                {
                  temp_rawcontent.set(fee576_ch,tdc,lopost,lopre,hipost,hipre);
                  mpcraw->addTower( temp_rawcontent );
                }
            }
        }

      //mpcraw->print();

      delete p;
    }

  /*=== Now we are only writing out the mpcSamples.
    === If we want to write out MpcRaw2, we can comment this out
  // If we found new electronics, get the amplitudes
  if ( mpcsamples->size()>0 )
    {
      GetAmplitudes( topNode );
    }
  */

  return True;
}

// Get the ADC Amplitudes and Times from the new MPC electronics
PHBoolean MpcEvent::GetAmplitudes(PHCompositeNode* topNode)
{
  if ( !mpcraw2 )mpcraw2 = findNode::getClass<mpcRawContainer>(topNode,"MpcRaw2");
  if (!mpcraw2)
    {
      cout << PHWHERE << "Unable to get MpcRaw2, is Node missing?" << endl;
      return False;
    }

  if (!mpcsamples ) mpcsamples = findNode::getClass<mpcSampleContainer>(topNode,"mpcSampleContainer");
  if (!mpcsamples)
    {
      cout << PHWHERE << "Unable to get mpcSampleContainer, is Node missing?" << endl;
      return False;
    }

  int adcsamp[NFEECH][NSAMPLES] = {{0}};
  int chan_list[NFEECH] = {0};
  for(int i=0; i<NFEECH; i++)
    {
      chan_list[i]=0;
      for(int j=0; j<NSAMPLES; j++)
        {
          adcsamp[i][j]=0;
        }
    }

  short nentries = mpcsamples->get_nentries();

  for(int isample=0; isample<nentries; isample++)
    {
      mpcSample *samp = mpcsamples->GetSample(isample);
      if ( samp==0 ) continue;
      int ch = samp->get_ch();
      if( mpcmap->getGridX(ch)<0 ) continue;
      int sample = (int) samp->get_sample();
      adcsamp[ch][sample] = samp->get_adc();
      chan_list[ch]=1;
    }

  mpcRawContentV5 temp_mpcraw2;

  for(int ch=0; ch<NFEECH; ch++)
    {
      if( chan_list[ch]==0 ) continue;
/*
      int adc[NSAMPLES];

      for(int isamp=0; isamp<NSAMPLES; isamp++)
        {
          adc[isamp]=adcsamp[ch][isamp];
        }
*/

      Float_t amplitude=0;
      Float_t amp1=0;
      Float_t amp2=0;
      Short_t tdc=0;
      Short_t tdc1=0;
      Short_t tdc2=0;
      Float_t fquality=0;
      Float_t ZSM = 0.0; 

      //GetAmplitude_algo1(adc,ch,amplitude,tdc,fquality);
      GetAmplitude_algo2(ch,adcsamp[ch],amplitude,tdc,fquality,amp1,amp2,tdc1,tdc2,ZSM);

      temp_mpcraw2.set_ch(ch);
      temp_mpcraw2.set_tdc(tdc);
      temp_mpcraw2.set_tdc1(tdc1);
      temp_mpcraw2.set_tdc2(tdc2);
      temp_mpcraw2.set_adc(amplitude);
      temp_mpcraw2.set_adc1(amp1);
      temp_mpcraw2.set_adc2(amp2);
      temp_mpcraw2.set_quality(fquality);
      temp_mpcraw2.set_ZSM(ZSM); 

      //cout << "zzz " << ch << "\t" << amplitude << "\t" << tdc << "\t" << fquality << endl;                   
      mpcraw2->addTower(temp_mpcraw2);

    }

  return True;
}

Double_t MpcEvent::SplineFitFcn(Double_t *x, Double_t *par)
{
  // par[0] is the amplitude (relative to the spline amplitude)
  // par[1] is the start time (in sample number)
  // par[2] is an overall offset
  // x[0] is in sample number
  Double_t xx = x[0]-par[1];
  Double_t f = 0.;

  int verbose = 0;

  // When fit is out of limits of good part of spline, ignore fit
  if ( (xx < fit_shape->get_start_time()) || (xx > fit_shape->get_end_time()) )
    {
      TF1::RejectPoint();
      if ( xx < fit_shape->get_start_time() )
        {
          //Double_t x0,y0;
          Double_t y0 = fit_shape->getValue(0);
          return par[0]*y0 + par[2];
        }
      else if ( xx > fit_shape->get_end_time() )
        {
          //Double_t x0,y0;
          int max_samp = fit_shape->get_nsamples();
          if ( verbose ) cout << "max_samp " << max_samp << endl;
          Double_t y0 = fit_shape->getValue(max_samp-1);
          return par[0]*y0 + par[2];
        }
    }

  // find the index in the vector which is closest to xx
  Double_t step = (fit_shape->get_end_time()-fit_shape->get_start_time())/fit_shape->get_nsamples();
  Double_t index = (xx-fit_shape->get_start_time())/step;

  int ilow = TMath::FloorNint( index );
  int ihigh = TMath::CeilNint( index );
  if ( ilow<0 || ihigh>=fit_shape->get_nsamples() )
    {
      if ( verbose )
        {
          cout << "ERROR, ilow ihigh " << ilow << "\t" << ihigh << endl;
          cout << " " << xx << " " << x[0] << " " << par[1] << endl;
        }
      if ( ilow<0 ) ilow = 0;
      else if ( ihigh>=fit_shape->get_nsamples() ) ihigh = (fit_shape->get_nsamples()-1);
    }

  if ( ilow==ihigh )
    {
      f = par[0]*fit_shape->getValue(ilow) + par[2];
    }
  else
    {

      // Linear Interpolation of spline
      Double_t x0 = 0.;
      Double_t y0 = 0.;
      Double_t x1 = 0.;
      Double_t y1 = 0.;

      x0 = fit_shape->get_start_time() + ilow*step;
      y0 = fit_shape->getValue(ilow);
      x1 = fit_shape->get_start_time() + ihigh*step;
      y1 = fit_shape->getValue(ihigh);
      f = par[0]*(y0+((y1-y0)/(x1-x0))*(xx-x0)) + par[2];
    }

  return f;
}

Double_t MpcEvent::SplineErrFcn(Double_t *x, Double_t *par)
{
  // par[0] is the amplitude (relative to the spline amplitude)
  // par[1] is the start time (in sample number)
  // x[0] is in sample number
  Double_t xx = x[0]-par[1];
  Double_t f = 0.;

  int verbose = 0;

  // When fit is out of limits of good part of spline, ignore fit
  if ( (xx < fit_sherr->get_start_time()) || (xx > fit_sherr->get_end_time()) )
    {
      TF1::RejectPoint();
      if ( xx < fit_sherr->get_start_time() )
        {
          //Double_t x0,y0;
          Double_t y0 = fit_sherr->getValue(0);
          return y0;
        }
      else if ( xx > fit_sherr->get_end_time() )
        {
          //Double_t x0,y0;
          int max_samp = fit_sherr->get_nsamples();
          if ( verbose ) cout << "max_samp " << max_samp << endl;
          Double_t y0 = fit_sherr->getValue(max_samp-1);
          return y0;
        }
    }

  // find the index in the vector which is closest to xx
  Double_t step = (fit_sherr->get_end_time()-fit_sherr->get_start_time())/fit_sherr->get_nsamples();
  Double_t index = (xx-fit_sherr->get_start_time())/step;

  int ilow = TMath::FloorNint( index );
  int ihigh = TMath::CeilNint( index );
  if ( ilow<0 || ihigh>=fit_sherr->get_nsamples() )
    {
      if ( verbose )
        {
          cout << "ERROR, ilow ihigh (ERR)" << ilow << "\t" << ihigh << endl;
          cout << " " << xx << " " << x[0] << " " << par[1] << endl;
        }
      if ( ilow<0 ) ilow = 0;
      else if ( ihigh>=fit_sherr->get_nsamples() ) ihigh = (fit_sherr->get_nsamples()-1);
    }

  if ( ilow==ihigh )
    {
      f = fit_sherr->getValue(ilow);
    }
  else
    {

      // Linear Interpolation of spline
      Double_t x0 = 0.;
      Double_t y0 = 0.;
      Double_t x1 = 0.;
      Double_t y1 = 0.;

      x0 = fit_sherr->get_start_time() + ilow*step;
      y0 = fit_sherr->getValue(ilow);
      x1 = fit_sherr->get_start_time() + ihigh*step;
      y1 = fit_sherr->getValue(ihigh);
      f = (y0+((y1-y0)/(x1-x0))*(xx-x0));
    }

  return f;
}

Double_t MpcEvent::SplineDoublePulseFitFcn(Double_t *x, Double_t *par)
{
  // par[0] is the amplitude of first pulse (relative to the spline amplitude)
  // par[1] is the start time of first pulse (in sample number)
  // par[2] is an overall offset
  // par[3] is the amplitude of second pulse (relative to the spline amplitude)
  // par[4] is the start time of second pulse (in sample number)

  // x[0] is in sample number

  Double_t f = 0.;
  Double_t step = (fit_shape->get_end_time()-fit_shape->get_start_time())/fit_shape->get_nsamples();

  int verbose = 0;

  // First pulse

  Double_t xx1 = x[0]-par[1];
  if ( (xx1<fit_shape->get_start_time()) || (xx1>fit_shape->get_end_time()) ){
    TF1::RejectPoint();
    if ( xx1 < fit_shape->get_start_time() )
      {
	Double_t y0 = fit_shape->getValue(0);
	f += par[0]*y0 + par[2];
      }
    else if ( xx1 > fit_shape->get_end_time() )
      {
	int max_samp = fit_shape->get_nsamples();
	Double_t y0 = fit_shape->getValue(max_samp-1);
	f += par[0]*y0 + par[2];
      }
  }
  else{

    // find the index in the vector which is closest to xx
    Double_t index1 = (xx1-fit_shape->get_start_time())/step;

    int ilow1 = TMath::FloorNint( index1 );
    int ihigh1 = TMath::CeilNint( index1 );

    if ( (ilow1<0) || (ihigh1>=fit_shape->get_nsamples()) )
      {
	if ( verbose )
	  {
	    cout << "ERROR, ilow1 ihigh1 " << ilow1 << "\t" << ihigh1 << endl;
	    cout << " " << xx1 << " " << x[0] << " " << par[1] << endl;
	  }
	if ( ilow1<0 ) ilow1 = 0;
	else if ( ihigh1>=fit_shape->get_nsamples() ) ihigh1 = (fit_shape->get_nsamples()-1);
      }

    if ( ilow1==ihigh1 )
      {
	f += par[0]*fit_shape->getValue(ilow1) + par[2];
      }
    else
      {
	// Linear Interpolation of spline
	Double_t x0 = 0.;
	Double_t y0 = 0.;
	Double_t x1 = 0.;
	Double_t y1 = 0.;

	x0 = fit_shape->get_start_time() + ilow1*step;
	y0 = fit_shape->getValue(ilow1);
	x1 = fit_shape->get_start_time() + ihigh1*step;
	y1 = fit_shape->getValue(ihigh1);
	f += par[0]*(y0+((y1-y0)/(x1-x0))*(xx1-x0)) + par[2];
      }

  }

  // Second pulse

  if(par[3]>0.0){

    Double_t xx2 = x[0]-par[4];
    if ( (xx2 < fit_shape->get_start_time()) || (xx2 > fit_shape->get_end_time()) ){
      TF1::RejectPoint();
      if ( xx2 < fit_shape->get_start_time() )
	{
	  Double_t y0 = fit_shape->getValue(0);
	  f += par[3]*y0;
	}
      else if ( xx2 > fit_shape->get_end_time() )
	{
	  int max_samp = fit_shape->get_nsamples();
	  Double_t y0 = fit_shape->getValue(max_samp-1);
	  f += par[3]*y0;
	}
    }
    else{

      // find the index in the vector which is closest to xx
      Double_t index2 = (xx2-fit_shape->get_start_time())/step;

      int ilow2 = TMath::FloorNint( index2 );
      int ihigh2 = TMath::CeilNint( index2 );

      // Second pulse

      if ( (ilow2<0) || (ihigh2>=fit_shape->get_nsamples()) )
	{
	  if ( verbose )
	    {
	      cout << "ERROR, ilow2 ihigh2 " << ilow2 << "\t" << ihigh2 << endl;
	      cout << " " << xx2 << " " << x[0] << " " << par[4] << endl;
	    }
	  if ( ilow2<0 ) ilow2 = 0;
	  else if ( ihigh2>=fit_shape->get_nsamples() ) ihigh2 = (fit_shape->get_nsamples()-1);
	}

      if ( ilow2==ihigh2 )
	{
	  f += par[3]*fit_shape->getValue(ilow2);
	}
      else
	{
	  // Linear Interpolation of spline
	  Double_t x0 = 0.;
	  Double_t y0 = 0.;
	  Double_t x1 = 0.;
	  Double_t y1 = 0.;

	  x0 = fit_shape->get_start_time() + ilow2*step;
	  y0 = fit_shape->getValue(ilow2);
	  x1 = fit_shape->get_start_time() + ihigh2*step;
	  y1 = fit_shape->getValue(ihigh2);
	  f += par[3]*(y0+((y1-y0)/(x1-x0))*(xx2-x0));
	}

    }

  }

  return f;
}

bool MpcEvent::FitOK()
{

  TString Converged = "CONVERGED "; 
  TString Failed = "FAILED "; 
  TString NotPosDef = "NOT POSDEF "; 
  TString Ok = "OK "; 

  TString fStat = gMinuit->fCstatu; 
  if(fStat.EqualTo(Converged) ||
     fStat.EqualTo(Ok) ||
     fStat.EqualTo(NotPosDef ) ){
    return true;
  }
  else 
    return false; 

}

int MpcEvent::GetAmplitude_algo2(const int ch, const int *adc, Float_t &amp, Short_t &tdc, Float_t &fquality, 
				 Float_t &amp1, Float_t &amp2, Short_t &tdc1, Short_t &tdc2, Float_t &ZSM)
{
  //static bool firstTime = true;

  if ( ch<0 || ch>=NFEECH )
    {
      cout << PHWHERE << " BIG ERROR, bad channel: " << ch << endl;
      exit(1);
    }

  Float_t ped = mpccalib->get_ped(ch);
  Float_t pedrms = mpccalib->get_pedrms(ch);

  Float_t x[NSAMPLES] = {0.}; 
  Float_t adcsub[NSAMPLES] = {0.}; 
  Float_t adcerr[NSAMPLES] = {0.}; 
  Int_t maxadc = -2048;  // make large for negative offsets!
  Int_t peak_samp = -1;
  for (int isamp=0; isamp<NSAMPLES; isamp++)
    {
      x[isamp] = (Float_t)isamp;
      adcsub[isamp] = adc[isamp] - ped;
      adcerr[isamp] = pedrms;
      if ( adcsub[isamp]>maxadc )
        {
          maxadc = adcsub[isamp];
          peak_samp = isamp;
        }
    }

  // adjust maxadc for the potential offset
  // (required for large negative offsets)

  maxadc = maxadc - adcsub[0]; 

  // record the zero suppression metric for this channel

  ZSM = adcsub[8] - adcsub[0]; 

  // look for second local maximum

  Int_t maxadc2 = -2048; 
  Int_t peak_samp2 = -1; 

  for (int isamp=1; isamp<NSAMPLES-1; isamp++)
    {
      if((adcsub[isamp]>maxadc2) &&
	 (adcsub[isamp]>adcsub[isamp-1]) &&
	 (adcsub[isamp]>adcsub[isamp+1]) &&
	 (isamp != peak_samp)){

	   maxadc2 = adcsub[isamp]; 
	   peak_samp2 = isamp; 

	 }
    }

  if(peak_samp2>0) maxadc2 = maxadc2 - adcsub[0]; 

  int verbose = 0;
  //verbose = 100;	// uncomment to see fits
  
  // We need two copies of the pulse data

  TGraphErrors g_subpulse(NSAMPLES,x,adcsub,0,adcerr);
  TGraphErrors g_subpulse2(NSAMPLES,x,adcsub,0,adcerr);
  if ( verbose>10 )
    {
      g_subpulse.SetMarkerStyle(20);
      g_subpulse.SetMarkerColor(2);
      g_subpulse.SetMarkerSize(0.8);
      g_subpulse.SetLineColor(2);
      g_subpulse2.SetMarkerStyle(20);
      g_subpulse2.SetMarkerColor(2);
      g_subpulse2.SetMarkerSize(0.8);
      g_subpulse2.SetLineColor(2);
    }

  fit_shape = fit_pshape[ch];
  fit_sherr = fit_psherr[ch];

  // Declare the fit functions we will need (static)

  static TF1 *fitfcn = new TF1("fitfcn",this,&MpcEvent::SplineFitFcn,0.,11.,3,"MpcEvent1","SplineFitFcn");
  fitfcn->SetRange(0.,11.);
  static TF1 *fitfcn2 = new TF1("fitfcn2",this,&MpcEvent::SplineDoublePulseFitFcn,0.,11.,5,"MpcEvent2","SplineDoublePulseFitFcn");
  fitfcn2->SetRange(0.,11.);

  // First fit to a single pulse

  fitfcn->SetParLimits(0,0.0,5.0*maxadc);  // pulse amplitude must be positive
  fitfcn->SetParameters(maxadc,peak_samp-7.0,adcsub[0]);
  if(verbose>10)
    g_subpulse.Fit(fitfcn,"RQO");
  else
    g_subpulse.Fit(fitfcn,"RNQ");

  bool FirstFitOK = FitOK();
  bool SecondFitOK = false; 

  // Second fit to a double-pulse (if indicated)

  if(FirstFitOK && (peak_samp2>0)){

    // Copy over the single pulse fit parameters.
    
    fitfcn2->SetParameters(maxadc,peak_samp-7.0,adcsub[0],maxadc2,peak_samp2-7.0);

    // Limit the pulse amplitudes to be positive. 

    fitfcn2->SetParLimits(0,0.0,5.0*maxadc);
    fitfcn2->SetParLimits(3,0.0,5.0*maxadc2);

    if(verbose>10)
      g_subpulse2.Fit(fitfcn2,"RQO");
    else
      g_subpulse2.Fit(fitfcn2,"RNQ");

    SecondFitOK = FitOK(); 

  }

  // Store fit values
  amp = static_cast<Float_t>( fitfcn->GetParameter(0) );
  amp1 = amp; 
  amp2 = 0.0; 
  fquality = fitfcn->GetChisquare()/fitfcn->GetNDF();

  // Update values if double pulse

  if(SecondFitOK && ((fitfcn->GetChisquare()/fitfcn->GetNDF())>(fitfcn2->GetChisquare()/fitfcn2->GetNDF())) ) {
    amp = static_cast<Float_t>( fitfcn2->GetParameter(0) + fitfcn2->GetParameter(3) ); 
    amp2 = static_cast<Float_t>( fitfcn2->GetParameter(3) ); 
    fquality = fitfcn2->GetChisquare()/fitfcn2->GetNDF(); 
  }
    
  // For the tdc, we choose 360 tdc ticks per sample
  // These limits imply a set of TDC cuts for valid hits, 
  Float_t samp_number = 7.0+fitfcn->GetParameter(1);
  if ( samp_number<0. ) 
    {
      samp_number = 0.;
    }
  else if ( samp_number>18.0 )
    {
      samp_number = 18.0;
    }
  tdc = static_cast<Short_t>( samp_number*360. );
  tdc1 = tdc; 
  tdc2 = -1.0; 

  if(SecondFitOK && ((fitfcn->GetChisquare()/fitfcn->GetNDF())>(fitfcn2->GetChisquare()/fitfcn2->GetNDF())) ) {

    Float_t samp_number = 7.0+fitfcn2->GetParameter(4);
    if ( samp_number<0. ) 
      {
	samp_number = 0.;
      }
    else if ( samp_number>18.0 )
      {
	samp_number = 18.0;
      }
    tdc2 = static_cast<Short_t>( samp_number*360. );

    if(tdc2<tdc1) tdc = tdc2; 

  }

  // Plot fits on specific conditions (DEBUG)

  if ( (verbose>10) && (amp<0.0) ) {

      static TCanvas *ac = new TCanvas("ac","ac",800,600);
      ac->Clear(); 
      gStyle->SetOptFit(1111);

      TString name = "ch "; name += ch;
      g_subpulse.SetTitle(name);
      g_subpulse2.SetTitle(name);

      if(SecondFitOK && ((fitfcn->GetChisquare()/fitfcn->GetNDF())>(fitfcn2->GetChisquare()/fitfcn2->GetNDF())) ) {
	TGraphErrors *leakMe = new TGraphErrors(g_subpulse2); 
 	leakMe->SetLineColor(4);
	leakMe->SetMarkerColor(4);
	leakMe->Draw("ap");
      }
      else{
        TGraphErrors *leakMe = new TGraphErrors(g_subpulse); 
	leakMe->Draw("ap");
      }

      gPad->Modified();
      gPad->Update();

      string junk;
      cin >> junk;
      ac->SaveAs(Form("Saved_Event_%i_%i.png",ch,EventNumber)); 

  }

  /*
  if ( verbose>10 ) {

      static TCanvas *ac = new TCanvas("ac","ac",8000,8000);
      if(NewEvent){

	if(!firstTime){
          string junk;
          cin >> junk;
	  ac->SaveAs(Form("Saved_Event_%i.png",EventNumber)); 
	}
	firstTime = false; 

	ac->Clear(); 
	gStyle->SetOptFit(1111); 
	ac->Divide(18,18);
	NewEvent = false; 
      }
      ac->cd(canIndex[mpcmap->getGridX(ch)][mpcmap->getGridY(ch)]);
      TString name = "ch "; name += ch;
      g_subpulse.SetTitle(name);
      g_subpulse2.SetTitle(name);

      // This leaks memory, but necessary to plot the full event
      // Better double-pulse fit?
      if(SecondFitOK && ((fitfcn->GetChisquare()/fitfcn->GetNDF())>(fitfcn2->GetChisquare()/fitfcn2->GetNDF())) ) {
	TGraphErrors *leakMe = new TGraphErrors(g_subpulse2); 
 	leakMe->SetLineColor(4);
	leakMe->SetMarkerColor(4);
	leakMe->Draw("ap");
      }
      else{
        TGraphErrors *leakMe = new TGraphErrors(g_subpulse); 
	leakMe->Draw("ap");
      }
      
      gPad->Modified();
      gPad->Update();

  }
  */

  return 1;
}

void MpcEvent::GetShapes()
{
  for (int ifeech=0; ifeech<576; ifeech++)
    {
      fit_pshape[ifeech] = mpccalib->get_pshape(ifeech);
      fit_psherr[ifeech] = mpccalib->get_psherr(ifeech);
      fit_np1shape[ifeech] = mpccalib->get_np1shape(ifeech);
      fit_np1sherr[ifeech] = mpccalib->get_np1sherr(ifeech);
    }
}

/*
int MpcEvent::GetAmplitude_algo1(int adc[NSAMPLES], int ch, float &amp, int &tdc, short &fquality)
{

  float num=0;
  float denom=0;
  for(int i=0; i<3; i++)
    {
      if( pedrms[ch][i]==0 ) continue;
      num += adc[i]/pedrms[ch][i]/pedrms[ch][i];
      denom += 1/pedrms[ch][i]/pedrms[ch][i];
    }

  float constant = num/denom;
  float chi2=0;
  int ndf=0;
  for(int i=0; i<3; i++)
    {
      if( pedrms[ch][i]==0 ) continue;
      chi2+=(adc[i]-constant)*(adc[i]-constant)/pedrms[ch][i]/pedrms[ch][i];
      ndf++;
    }
  ndf-=1;

  amp=0;
  int imax=0;
  int max=0;
  for(int i=3; i<8; i++)
    {
      amp += adc[i]-constant;
      if( adc[i]>max )
        {
          imax=i;
          max=adc[i];
        }
    }

  tdc= (int) 17.8*(imax-3);

  float chi2ndf=chi2/ndf;
  fquality=(chi2ndf>0.8);

  return 1;
}
*/

// Convert the mpcRaw data (ADC's) to mpcTowers (energies)
PHBoolean MpcEvent::CalibrateTowers(PHCompositeNode* topNode)
{
  mpcraw = findNode::getClass<mpcRawContainer>(topNode,"MpcRaw");
  mpcraw2 = findNode::getClass<mpcRawContainer>(topNode,"MpcRaw2");
  if (mpcraw==0 && mpcraw2==0)
    {
      static int count = 0;
      if ( count<10 )
        {
          cout << PHWHERE << "Unable to get MpcRaw or MpcRaw2, are Nodes missing?" << endl;
          count++;
        }
      return False;
    }

  mpctower = findNode::getClass<mpcTowerContainer>(topNode,"mpcTowerContainer");
  if (!mpctower)
    {
      cout << PHWHERE << "Unable to get mpcTowerContainer, is Node missing?" << endl;
      return False;
    }

  // Keep track of channels that don't have gain values
  static int nbadgains[NFEECH] = {0};

  // Get the mpcraw data from the old electronics and apply the gain
  // and other calibrations.

  mpcTowerContentV1 temp_towercontent;
  int post_amu = 0;
  int pre_amu = 0;
  int tdc_amu = 0;
  if ( mpcraw != 0 )
    {
      post_amu = mpcraw->get_post_amu();
      pre_amu = mpcraw->get_pre_amu();
      tdc_amu = mpcraw->get_tdc_amu();
      mpctower->set_amu(tdc_amu,pre_amu,post_amu);
    }

  for (unsigned int iraw=0; mpcraw!=0 && iraw<mpcraw->size(); iraw++)
    {
      mpcRawContent *tower = mpcraw->getTower( iraw );

      int fee576ch = tower->get_ch();
      int xpos = mpcmap->getGridX( fee576ch );
      int ypos = mpcmap->getGridY( fee576ch );
      if ( xpos<0 || xpos>17 || ypos<0 || ypos>17 )
        {
          //cout << "xxx " << fee576ch << "\t" << xpos << "\t" << ypos << endl;
          continue;
        }

      int lopost = tower->get_lopost();
      int lopre = tower->get_lopre();
      int hipost = tower->get_hipost();
      int hipre = tower->get_hipre();

      // pedestal correction
      float loped = mpccalib->get_ped_lgpost(fee576ch,post_amu) - mpccalib->get_ped_lgpre(fee576ch,pre_amu);
      float hiped = mpccalib->get_ped_hgpost(fee576ch,post_amu) - mpccalib->get_ped_hgpre(fee576ch,pre_amu);
      float hilo_ratio = mpccalib->get_hilo_ratio(fee576ch);
      float hilo_limit_rms = mpccalib->get_hilo_limit_rms(fee576ch);
      float hilo_limit = mpccalib->get_hilo_limit(fee576ch)-6.0*hilo_limit_rms;

      // cut hot channels here????

      // cuts here
      // 234 MeV = MIP peak position
      // what is currently stored in the gains is relative to the mip peak position
      float gain = mpccalib->get_adc_gain(fee576ch);
      if ( gain<=0. )
        {
          if ( nbadgains[fee576ch]<3 )
            {
              cout << PHWHERE << " WARNING, gain for " << fee576ch << " is " << gain << endl;
              nbadgains[fee576ch]++;
            }
          gain = 0.;
        }

      float tof = tower->get_tdc();

      float energy = -9999.;
      if ( (lopost-lopre)>hilo_limit )
        {
          // use the low gain data
          energy = (lopost-lopre-loped)*gain;
        }
      else
        {
          // use the high gain data
          energy = (hipost-hipre-hiped)*gain/hilo_ratio;
        }

/*
      if ( energy<-5.0 ) 
        {
          cout << "BAD E " << fee576ch << "\t" << energy << "\t"
               << lopost << "\t" << lopre << "\t" << gain << "\t"
               << loped << endl;
        }
*/

      // Apply the pi0 or other gain correction
      if ( mpc_gaincorr_flag > 0 )
        {
          // commenting this out temporarily.  use hardcoded values instead.
          //energy *= mpccalib->get_gaincorr( fee576ch );
          //cout << "gaincorr\t" << fee576ch << "\t" << mpccalib->get_gaincorr( fee576ch ) << endl;
        }

      // Hardcoded corrections from D. Kleinjan in south mpc
      int arm = mpcmap->getArm( fee576ch );
      if ( arm==0 )
        {
          if ( RunNumber>=258249 && RunNumber<=258655 ) 
            {
              energy *= RunEnergyCorr;
            }
          else if ( RunNumber>=259365 && RunNumber<=259575 ) 
            {
              energy *= RunEnergyCorr;
            }
        }

      // correct using led
      float ledcorr = mpccalib->get_led( fee576ch );
      if ( ledcorr > 0. )
        {
          energy = energy/ledcorr;
        }

      temp_towercontent.set(fee576ch,tof,energy);
      mpctower->addTower( temp_towercontent );

    }

  // Now process the new electronics
  for (unsigned int iraw=0; mpcraw2!=0 && iraw<mpcraw2->size(); iraw++)
    {
      mpcRawContent *raw = mpcraw2->getTower(iraw);
      int fee576ch = raw->get_ch();

      // For the time, we need to use database calibs.
      // For now, we just hard code it.
      float tof = raw->get_sample()*17.762;
      if (fee576ch<288) tof = tof - 119.5;
      else        tof = tof - 104.5;

      float gain = mpccalib->get_adc_gain(fee576ch);
      if ( gain<=0. )
        {
          if ( nbadgains[fee576ch]<3 )
            {
              cout << PHWHERE << " WARNING, gain for " << fee576ch << " is " << gain << endl;
              nbadgains[fee576ch]++;
            }
          gain = 0.;
        }

      float energy = raw->get_adc()*gain;

      temp_towercontent.set(fee576ch,tof,energy);
      mpctower->addTower( temp_towercontent );
    }

  // Apply the MpcScale correction
  if ( reco_consts->FlagExist("MPCSCALE_NORTH") )
    {
      Float_t escale = reco_consts->get_FloatFlag("MPCSCALE_NORTH");
      mpctower->scale( escale, 1 );
    }
  if ( reco_consts->FlagExist("MPCSCALE_SOUTH") )
    {
      Float_t escale = reco_consts->get_FloatFlag("MPCSCALE_SOUTH");
      mpctower->scale( escale, 0 );
    }

  // Now apply event by event common mode noise suppression
  if ( RunNumber>=205000 && RunNumber<=207000 )	// Run06
    {
      SubtractDriverNoise(1);
      SubtractDriverNoise(7);
    }

  if ( mpc_verbosity>0 )
    {
      cout << PHWHERE << endl;
      mpctower->print();
    }

  return True;
}

void MpcEvent::SubtractDriverNoise(const int noisy_driver)
{
  vector<float> driver_e;
  vector<float> driver_ich;
  double mean = 0.;
  for (unsigned int ich = 0; ich < mpctower->size(); ich++)
    {
      mpcTowerContent *tower = mpctower->getTower(ich);

      int fee576ch = tower->get_ch();
      if ( mpcmap->isCrystal( fee576ch ) == 0 ) continue;

      int driver = mpcmap->getDriver( fee576ch );
      if ( driver!=noisy_driver ) continue;

      float energy = tower->get_energy();

      driver_e.push_back( energy );
      driver_ich.push_back( ich );
      mean += energy;
    }

  std::sort( driver_e.begin(), driver_e.end() );
  int ntowers = driver_e.size();
  if ( ntowers>10 )
    {
      mean = mean/ntowers;
      double trunc_mean = 0.;
      for (int i=2; i<ntowers-2; i++)
        {
          trunc_mean += driver_e[i];
          //cout << "trunc_mean " << i << "\t" << driver_e[i] << endl; 
        }
      trunc_mean = trunc_mean/(ntowers-4);
/*
      cout << "DRIVER1E " << ntowers << " " << driver_e[0]
           << "\t" << driver_e[ntowers-1]
           << "\t" << mean
           << "\t" << trunc_mean
           << endl;
*/

      for (unsigned int i=0; i<driver_ich.size(); i++)
        {
          int ich = driver_ich[i];
          mpcTowerContent *tower = mpctower->getTower(ich);
          float e = tower->get_energy();
          e -= trunc_mean;
          tower->set_energy( e );
        }
    }
}

int MpcEvent::MakeOriginMap(PHCompositeNode* topNode)
{
  typedef struct {
    int mctrack;
    //int ntrack;
    int itparent;
    int idparent;
    int idpart;
    //float ptot;
  } kinhit;

  kinhit mctracklist[MAX_SIMTRACKS];
  for (int i=0; i<MAX_SIMTRACKS; i++)
    {
      mctracklist[i].mctrack = -1;
      mctracklist[i].itparent = -1;
      mctracklist[i].idparent = -1;
      mctracklist[i].idpart = -1;
    }

  // make mctrack (true_track) list
  int max_track = 0;
  size_t fkinrows = fkin->RowCount();

  //cout << "FKIN LIST " << fkinrows << endl;

  for (size_t ifkin=0; ifkin<fkinrows; ifkin++)
    {
      int mctrack = fkin->get_true_track(ifkin); 
      if ( mctrack<1 || mctrack>MAX_SIMTRACKS )
        {
          cout << PHWHERE << " ERROR, mctrack out of range " << mctrack << endl;
          continue;
        }
      if ( mctracklist[mctrack-1].mctrack > 0 ) continue;

      mctracklist[mctrack-1].mctrack  = mctrack;
      //mctracklist[mctrack-1].ntrack   = fkin->get_ntrack(ifkin);
      mctracklist[mctrack-1].itparent = fkin->get_itparent(ifkin);
      mctracklist[mctrack-1].idparent = fkin->get_idparent(ifkin);
      mctracklist[mctrack-1].idpart   = fkin->get_idpart(ifkin);
      //mctracklist[mctrack-1].ptot     = fkin->get_ptot(ifkin);

      //cout << ifkin
      //     << " " << fkin->get_itparent(ifkin)
      //     << " " << fkin->get_idparent(ifkin)
      //     << " " << fkin->get_idpart(ifkin)
      //     << endl;

      if ( mctrack > max_track ) 
        {
          max_track = mctrack;
        }
    }

  //cout << "maxtrack " << max_track << endl;
  for (int itrk=1; itrk<=max_track; itrk++)
    {
      int orig_track = -1;
      int orig_idpart = -1;
      int new_itparent = -1;
      int new_idparent = -1;

      int temp_index = itrk;

      do  
        {
          if ( temp_index<1 || temp_index>max_track )
            {
              new_idparent = 0;
              cout << "ERROR makemap " << itrk << "\t" << temp_index << "\t"
                   << new_itparent << "\t" << new_idparent << "\t"
                   << orig_track << "\t" << orig_idpart << endl;

              break;
            }
          new_itparent = mctracklist[temp_index-1].itparent;
          new_idparent = mctracklist[temp_index-1].idparent;
          orig_track = mctracklist[temp_index-1].mctrack;
          orig_idpart = mctracklist[temp_index-1].idpart;

          //cout << "makemap " << itrk << "\t" << temp_index << "\t"
          //     << new_itparent << "\t" << new_idparent << "\t"
          //     << orig_track << "\t" << orig_idpart << endl;

          temp_index = new_itparent;
        }
      while ( new_idparent!=0 );

      my_itorigin[itrk-1] = orig_track;
      my_idorigin[itrk-1] = orig_idpart;
    }
  
  return 1;
}

PHBoolean MpcEvent::CalibrateSimTowers(PHCompositeNode* topNode)
{
  mpctower = findNode::getClass<mpcTowerContainer>(topNode,"mpcTowerContainer");
  if (!mpctower)
    {
      cout << PHWHERE << "Unable to get mpcTowerContainer, is Node missing?" << endl;
      return False;
    }

  mpcgeatower = findNode::getClass<mpcGeaTowerContainer>(topNode,"mpcGeaTowerContainer");
  if (!mpcgeatower)
    {
      cout << PHWHERE << "Unable to get mpcGeaTowerContainer, is Node missing?" << endl;
      return False;
    }

  fkin = findNode::getClass<fkinWrapper>(topNode, "fkin");
  if (fkin==0)
    {
      cout << PHWHERE << "Unable to get fkin, is Node missing?" << endl;
      return -1;
    }

  MpcPISAHit *mpchits = MpcPISAHit::GetMpcHitEvt();
  Int_t mpcNhits = MpcPISAHit::GetMpcCount(); 

  //  cout << "Found " << mpcNhits << " hits" << endl;
  if ( mpchits==0 )
    {
      static int counter = 0;
      if ( counter<3 ) cout << PHWHERE << " ERROR: mpchits = 0" << endl; 
      ++counter;
      return False;
    }

  // Set up Monte Carlo evaluation object
  int result = MakeOriginMap(topNode);
  if ( result==0 ) return False;

  int ngeatow = 0;
  map<int,float> track_edep_in_tower;

  vector <mpcGeaTowerContentV1> geatowers;

/*
//Int_t mcTrack = pc1ghit->get_mctrack(pcghitID);
Int_t idPart; // GEANT particle ID of the mctrack particle
Int_t idParent; // GEANT particle of the parent of the mctrack particle
Float_t pTot; // total momentum (GeV/c) at the creation vertex of the mctrack particle
Float_t rVertex; // radial position (cm) of the creation vertex of the mctrack particle
Float_t zVertex; // longitudinal (beam axis, in cm) position of the creation vertex of the mctrack particle
Float_t pPhi; // azimuthal direction (degrees) of the mctrack particle vertex momentum
Float_t pTheta; // polar direction (degrees) of the mctrack particle vertex momentum
Int_t nFile; // PISA hits file number of the mctrack particle (for MERGED events)
Int_t itParent; // parent track number of the mctrack particle
Int_t itOrigin; // track number of mctrack primary ancestor
Int_t idOrigin; // GEANT particle ID of the mctrack primary ancestory
*/

  // For each hit we find the tower it belongs in, and sum the energy for that tower.
  // For now we ignore the timing information...
  const int N_ARMS = 2;
  const int MAX_X = 18;
  const int MAX_Y = 18;
  Double_t grid_etemp[N_ARMS][MAX_X][MAX_Y] = {{{0.}}};
  Float_t  grid_toftemp[N_ARMS][MAX_X][MAX_Y] = {{{9999.}}};

  //cout << "mpcNhits\t" << mpcNhits << endl;
  for (int ihit=0; ihit<mpcNhits; ihit++)
    {
      //GeaTrkStack(mcTrack, idPart, idParent, pTot, rVertex, zVertex, pTheta, pPhi, nFile, itParent, itOrigin, idOrigin); 
      //mpchits[ihit].Print();
      //cout << itOrigin << "\t" << idOrigin << endl;

      Int_t mcTrack = mpchits[ihit].GetMctrack();	// same as true_track
      Int_t tower_id = mpchits[ihit].GetTowerID();
      Float_t dedx = mpchits[ihit].GetDedx();
      Float_t tofg = mpchits[ihit].GetTofg();
      Float_t idincoming = mpchits[ihit].GetP_id();
      Float_t itincoming = mpchits[ihit].GetPNum();
      Float_t zz_mpc = mpchits[ihit].GetZin();

      if ( tower_id<0||tower_id>=2*18*18 )
        {
          cout << "MpcEvent::CalibrateSimTowers, bad towerid " << tower_id << endl;
        }

      int arm = 0;
      if ( zz_mpc>0. ) arm = 1;
      int xpos = (tower_id%18);
      if ( arm==0 )
        {
          xpos = 17 - xpos;
        }
      //int sign = (arm==1) ? -1 : 1;
      int ypos = tower_id/18 - arm*18;

      if ( xpos<0 || xpos>17 || ypos<0 || ypos>17 || arm<0 || arm>1 )
        {
          cout << "MAJOR ERROR, xpos,ypos,arm = "
               << xpos << " " << ypos << " " << arm << endl;
        }

      grid_etemp[arm][xpos][ypos] += dedx;	// dedx is in keV

      // for now, we use the earliest hit time
      if ( tofg < grid_toftemp[arm][xpos][ypos] )
        {
          grid_toftemp[arm][xpos][ypos] = tofg;
        }

      // Get primary itrack and pid using my homebrew ancestry
      //cout << my_itorigin[mcTrack-1] << "\t" << my_idorigin[mcTrack-1] << endl;	// my homebrew ancestry
      Int_t itOrigin = my_itorigin[mcTrack-1];
      Int_t idOrigin = my_idorigin[mcTrack-1];
      int index = itOrigin*100000 + arm*10000 + xpos*100 + ypos;

      if ( track_edep_in_tower[index] == 0. )
        {
          // first time through this tower, store 
          mpcGeaTowerContentV1 temp_gea;

          //cout << "mpcmap " << xpos << "\t" << ypos << "\t" << arm << endl;
          int ch = mpcmap->getFeeCh(xpos,ypos,arm);

          //temp_gea.set_ch( mpcmap->getFeeCh(xpos,ypos,arm) );
          temp_gea.set_ch( ch );
          temp_gea.set_itincoming( (Int_t)itincoming );
          temp_gea.set_idincoming( (Int_t)idincoming );
          temp_gea.set_itorigin( itOrigin );
          temp_gea.set_idorigin( idOrigin );
          geatowers.push_back( temp_gea );

          track_edep_in_tower[index] = dedx*1e-6;
          ngeatow++;
        }
      else
        {
          track_edep_in_tower[index] += (dedx*1e-6);
        }
    }

  mpcSimTowerContentV1 temp_towercontent;

  //const float leakage = 1.05;	// effect from leakage out rear

  float esum = 0.;
  for (int iarm=0; iarm<N_ARMS; iarm++)
    {
      for (int ix=0; ix<MAX_X; ix++)
        {
          for (int iy=0; iy<MAX_Y; iy++)
            {
              if ( grid_etemp[iarm][ix][iy] > 0. )
                {
                  grid_etemp[iarm][ix][iy] *= 1e-6;	// convert grid_etemp to GeV

                  esum += grid_etemp[iarm][ix][iy];	// calculate sum over entire MPC

                  int fee576ch = mpcmap->getFeeCh(ix,iy,iarm);

                  if ( fee576ch<0 )	// sanity check
                    {
                      cout << PHWHERE << " CalibrateSimTowers ERROR, feech iarm ix iy "
                           << fee576ch << "\t" << iarm << "\t" << ix << "\t" << iy << endl;
                    }

                  /* We now do this on the fly in the container objects
                  // There are 1.45 p.e./MeV.  Put in stochastic term of 2.6%*E
                  float sigmaE = gsl_ran_gaussian(gsl_rand_gen,0.02626128)*sqrt(grid_etemp[iarm][ix][iy]);
                  float newE = grid_etemp[iarm][ix][iy] + sigmaE;	// photon fluctation term
                  newE += gsl_ran_gaussian(gsl_rand_gen,.075);	// 75 MeV of noise
                  temp_towercontent.set( fee576ch, grid_toftemp[iarm][ix][iy], newE );
                  */

                  temp_towercontent.set( fee576ch, grid_toftemp[iarm][ix][iy], grid_etemp[iarm][ix][iy] );
                  mpctower->addTower( temp_towercontent );
                }
            }
        }
    }

  //cout << "esum was " << esum << endl;
  for (int igt=0; igt<ngeatow; igt++)
    {
      // here we associate the primary with the edep
      // maybe we should associate the incoming with the edep?
      int feech = geatowers[igt].get_ch();
      int arm = mpcmap->getArm( feech );
      int xpos = mpcmap->getGridX( feech );
      int ypos = mpcmap->getGridY( feech );
      int itorigin = geatowers[igt].get_itorigin();
      int index = itorigin*100000 + arm*10000 + xpos*100 + ypos;
      geatowers[igt].set_edep( track_edep_in_tower[index] );
      float fraction = track_edep_in_tower[index]/grid_etemp[arm][xpos][ypos];
      geatowers[igt].set_fraction( fraction );
      mpcgeatower->addTower( geatowers[igt] );
    }

  return True;
}

PHBoolean MpcEvent::DoClustering(PHCompositeNode* topNode)
{
  static int nevts2 = 0;
  
  //  cout << PHWHERE << " IN CLUSTERING " << nevts2 << endl;
  nevts2++;
  mpctower = findNode::getClass<mpcTowerContainer>(topNode,"mpcTowerContainer");
  if (!mpctower)
    {
      cout << PHWHERE << "Unable to get mpcTowerContainer, is Node missing?" << endl;
      return False;
    }
  if ( mpctower->size()==0 ) return True;	// no towers found

  mpcclus = findNode::getClass<mpcClusterContainer>(topNode,"mpcClusterContainer");
  if (!mpcclus)
    {
      cout << PHWHERE << "Unable to get mpcClusterContainer, is Node missing?" << endl;
      return False;
    }

  // get bbc vertex... Try VtxOut first, and then PHGlobal, and then BbcOut
  float zvtx = -9999.;
  VtxOut *vtxout = findNode::getClass<VtxOut>(topNode,"VtxOut");
  if ( vtxout )
    {
      zvtx = vtxout->get_ZVertex();
    }
  else
    { 
      phglobal = findNode::getClass<PHGlobal>(topNode,"PHGlobal");
      if ( phglobal )
        {
          zvtx = phglobal->getBbcZVertex();
        }
      else
        {
          BbcOut *bbcout = findNode::getClass<BbcOut>(topNode,"BbcOut");
          if ( bbcout )
            {
              zvtx = bbcout->get_VertexPoint();
            }
          else
            {
              cout << PHWHERE << "Unable to get VtxOut, PHGlobal, and BbcOut, are Nodes missing?" << endl;
              return False;
            }
        }
    }

  if ( mpc_verbosity )
    {
      cout << "mpc_verbosity " << mpc_verbosity << endl;
      cout << "zvertex = " << zvtx << endl;
    }
  
  if ( fabs(zvtx) > 300. )
    {
      if ( fabs(mpc_recobadvtx) < 300. )
        {
          // Reco event, set zvtx to whatever default analyzer wants
          zvtx = mpc_recobadvtx;
        }
      else	// Skip Event
        {
          //  cout << "zvertex: " << zvtx << " is too large\n";
          return True;
        }
    }
 
  // set thresholds
  towerThresh = 0.01;
  if(mpc_cluster_alg == 0) towerThresh = 0.01;
  if(mpc_cluster_alg == 1)
    {
      towerThresh = 0.01;
      //towerThresh = 0.2;	// For run11pp?
    }


  peakThresh = 0.1;
  chi2limit = 2;

  if ( mpc_verbosity>0 )
    {
      cout << PHWHERE << endl;
      mpctower->print();
    }

  // set up display of cluster positions
  if ( mpc_event_display )
    {
      if ( s_cluster_position==0 )
        {
          s_cluster_position = new TGraph();
          s_cluster_position->SetMarkerStyle(5);
          s_cluster_position->SetMarkerSize(2);
          s_cluster_position->SetLineWidth(2);
        }
      s_cluster_position->Set(0);
    }

  if ( mpc_cluster_alg==0 )		// GAMS Algorithm
    {
      Cluster_GAMS_Algorithm(zvtx);
    }
  else if ( mpc_cluster_alg==1 )	// GAMS Tuned for MPC
    {
      Cluster_MpcGAMS_Algorithm(zvtx);
    }
  else if ( mpc_cluster_alg==2 )	// Andrey Kazantsev/PHOS Algorithm?
    {
    }

  // here we set tofhad to esum... need to fix this
/*
  if ( mpcclus->size()>0 )
    {
      mpcClusterContent *temp_clus = mpcclus->getCluster(0);
      temp_clus->set_tofhad( mpctower->get_esum() );
    }
*/

  return True;

}


PHBoolean MpcEvent::Cluster_MpcGAMS_Algorithm(const float zvtx)
{
/*
  if ( fMpcSector[0]==0 ) fMpcSector[0] = new MpcSectorRec(0);
  if ( fMpcSector[1]==0 ) fMpcSector[1] = new MpcSectorRec(1);
*/

  for (int iarm=0; iarm<2; iarm++)
    {
      fMpcSector[iarm]->SetTowerThreshold( towerThresh );
      fMpcSector[iarm]->SetPeakThreshold( peakThresh );
      fMpcSector[iarm]->SetChi2Limit( chi2limit );

      fMpcSector[iarm]->SetVertex( 0., 0., zvtx );
      fMpcSector[iarm]->FillHitList( mpctower );
      int nCl = fMpcSector[iarm]->FindClusters();

      vector<MpcCluster>* ClusterList = fMpcSector[iarm]->GetClusters();

      if ( mpc_verbosity>0 )
        {
          cout << PHWHERE << " nCl ntowers\t"
               << nCl << "\t" << mpctower->size() << endl;
        }

      if (nCl < 0)
        {
          cerr << PHWHERE << ": Increase parameter MaxLen !!!" << endl;
          return False;
        }

      // Fill cluster table, mpcclus
      // Start looping on clusters

      fMpcSector[iarm]->SetClusterOut( mpcclus );
      const int MAX_NUMBER_OF_PEAKS = 100;
      const float MinClusterEnergy = 0.5;	// 500 MeV

      MpcPeakarea pPList[MAX_NUMBER_OF_PEAKS];
      MpcModule peaks[MAX_NUMBER_OF_PEAKS];

      vector<MpcCluster>::iterator pc;
      for ( pc = ClusterList->begin(); pc != ClusterList->end(); ++pc )
        {
          int npk = (*pc).GetPeaks_Mpc(pPList, peaks);

          if ( mpc_verbosity>0 )
            {
              cout << PHWHERE << " GetPeaks() npeaks = " << npk << endl;
            }

          MpcPeakarea *pp = pPList;
          assert(pp!=0);

          for ( int ip = 0; ip < npk; ++ip )
            { 
              if ( mpc_verbosity>0 )
                {
                  cout << "In cluster " << ip << " write " << pp->GetTotalEnergy() << endl;
                }

              if (pp->GetTotalEnergy() > MinClusterEnergy && pp->GetTotalEnergy()<40000. ) 
                {
                  mpcClusterContent *ctemp = fMpcSector[iarm]->FillPeakArea(*pp,(*pc));

                  if ( mpc_event_display )
                    {
                      float x = ctemp->x();
                      float y = ctemp->y();
                      float ecore = ctemp->ecore();
                      if ( iarm==0 )
                        {
                          Int_t npt = s_cluster_position->GetN();
                          cout << "cluster draw clusnum x y ecore "
                               << npt << "\t" << x << "\t" << y << "\t" << ecore << endl;
                          //s_cluster_position->SetPoint(npt,x/2.26,y/2.26);
                          // plot by position
                          s_cluster_position->SetPoint(npt,x,y);
                        }
                    }
                }
              else if ( pp->GetTotalEnergy()>40000. )
                {
                  cout << PHWHERE << "ERROR, bad energy " << pp->GetTotalEnergy()
                       << " in event " << EventNumber << endl;
                }

              pp++;
            } // end of loop over peakareas of cluster (*pc)
        } // end loop over clusters of sector is

      if ( mpc_event_display )
        {
          if ( iarm==0 ) s_cluster_position->Draw("p");
          gPad->Modified();
          gPad->Update();
          string junk;
          cin >> junk;
        }

    } // end loop over arms

  return True;

}


PHBoolean MpcEvent::Cluster_GAMS_Algorithm(const float zvtx)
{
/*
  if ( fMpcSector[0]==0 ) fMpcSector[0] = new MpcSectorRec(0);
  if ( fMpcSector[1]==0 ) fMpcSector[1] = new MpcSectorRec(1);
*/



  for (int iarm=0; iarm<2; iarm++)
    {
      fMpcSector[iarm]->SetTowerThreshold( towerThresh );
      fMpcSector[iarm]->SetPeakThreshold( peakThresh );
      fMpcSector[iarm]->SetChi2Limit( chi2limit );

      fMpcSector[iarm]->SetVertex( 0., 0., zvtx );
      fMpcSector[iarm]->FillHitList( mpctower );
      int nCl = fMpcSector[iarm]->FindClusters();
      
      vector<MpcCluster>* ClusterList = fMpcSector[iarm]->GetClusters();
      
      if ( mpc_verbosity>0 )
        {
          cout << PHWHERE << " nCl ntowers\t"
               << nCl << "\t" << mpctower->size() << endl;
        }

      if (nCl < 0)
        {
          cerr << PHWHERE << ": Increase parameter MaxLen !!!" << endl;
          return False;
        }

      // Fill cluster table, mpcclus
      // Start looping on clusters

      fMpcSector[iarm]->SetClusterOut( mpcclus );
      const int MAX_NUMBER_OF_PEAKS = 100;
      const float MinClusterEnergy = 0.1;	// 100 MeV

      MpcPeakarea pPList[MAX_NUMBER_OF_PEAKS];
      MpcModule peaks[MAX_NUMBER_OF_PEAKS];

      vector<MpcCluster>::iterator pc;
      for ( pc = ClusterList->begin(); pc != ClusterList->end(); ++pc )
        {
          int npk = (*pc).GetPeaks(pPList, peaks);
          if ( mpc_verbosity>0 )
            {
              cout << PHWHERE << " GetPeaks() npeaks = " << npk << endl;
            }

          MpcPeakarea *pp = pPList;
          assert(pp!=0);

          for ( int ip = 0; ip < npk; ++ip )
            { 
              if ( mpc_verbosity>0 )
                {
                  cout << "In cluster " << ip << " write " << pp->GetTotalEnergy() << endl;
                }

              if (pp->GetTotalEnergy() > MinClusterEnergy && pp->GetTotalEnergy()<40000. ) 
                {
                  mpcClusterContent *ctemp = fMpcSector[iarm]->FillPeakArea(*pp,(*pc));

                  if ( mpc_event_display )
                    {
                      float x = ctemp->x();
                      float y = ctemp->y();
                      float ecore = ctemp->ecore();
                      if ( iarm==0 )
                        {
                          Int_t npt = s_cluster_position->GetN();
                          cout << "cluster draw clusnum x y ecore "
                               << npt << "\t" << x << "\t" << y << "\t" << ecore << endl;
                          //s_cluster_position->SetPoint(npt,x/2.26,y/2.26);
                          // plot by position
                          s_cluster_position->SetPoint(npt,x,y);
                        }
                    }
                }
              else if ( pp->GetTotalEnergy()>40000. )
                {
                  cout << PHWHERE << "ERROR, bad energy " << pp->GetTotalEnergy()
                       << " in event " << EventNumber << endl;
                }

              pp++;
            } // end of loop over peakareas of cluster (*pc)
        } // end loop over clusters of sector is

      if ( mpc_event_display )
        {
          if ( iarm==0 ) s_cluster_position->Draw("p");
          gPad->Modified();
          gPad->Update();
          string junk;
          cin >> junk;
        }

    } // end loop over arms

  return True;

}

