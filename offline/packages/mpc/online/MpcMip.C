//  GENERal PHENIX tools
#include "Fun4AllServer.h"
#include "getClass.h"
#include "PHCompositeNode.h"
#include "phool.h"
#include "Fun4AllHistoManager.h"
#include "Fun4AllReturnCodes.h"
#include "recoConsts.h"

//  Data classes I am using in analysis
//#include "PHCentralTrack.h"
//#include "PreviousEvent.h"
#include "TriggerHelper.h"
#include "TrigLvl1.h"
#include "PHGlobal.h"
#include "EventHeader.h"
#include "MpcMip.h"
#include "mpcRawContainer.h"
#include "mpcRawContent.h"
#include "BbcOut.h"
#include "BbcRaw.h"
#include "Bbc.hh"
#include "MpcCalib.h"
#include "BbcGeo.hh"
#include "MpcMap.h"
#include "MpcCalib.h"

//  Root histogram types
#include <TH1.h>
#include <TH2.h>
#include <TCanvas.h>
#include <TNtuple.h>
#include <TTree.h>
#include <TFile.h>
//#include <cmath>

using namespace std;
using namespace findNode;

static const float MM2CM = 0.1;	// mm to cm

MpcMip::MpcMip(const char* outhistfile, const char* outtreefile) : SubsysReco("MpcMip")
{
  //  Take the output file name as an argument
  OutHistFile= outhistfile;
  OutTreeFile= outtreefile;

  HistoManager = new Fun4AllHistoManager("MpcMip");

  bbcgeom = new BbcGeo();

  mpcmap = MpcMap::instance();
  mpccalib = MpcCalib::instance();

  treeflag = false;

  HIT_TOWER_MIN_HIADC=30.;
  DR_MAXIMUM_CUT=5;

  for(int ibit=0; ibit<nbit; ibit++) {
    lo_bit_cut[ibit]=0;
    hi_bit_cut[ibit]=ibit+2;
    cout << "bit: " << lo_bit_cut[ibit] << "\t" << hi_bit_cut[ibit] << endl;
  }

  lo_bit_cut[0]=0;
  lo_bit_cut[1]=0;
  lo_bit_cut[2]=0;
  hi_bit_cut[0]=2;
  hi_bit_cut[1]=3;
  hi_bit_cut[2]=4;

  lo_rad_cut[0]=0.;
  lo_rad_cut[1]=0.;
  lo_rad_cut[2]=0.;
  lo_rad_cut[3]=0.;
  hi_rad_cut[0]=2.;
  hi_rad_cut[1]=3.;
  hi_rad_cut[2]=4.;
  hi_rad_cut[3]=5.;

  return ;
}

MpcMip::~MpcMip()
{
  delete bbcgeom;
}

int MpcMip::InitRun(PHCompositeNode *topNode)
{
  trighelp = new TriggerHelper(topNode);
  if ( trighelp==0 )
    {
      cout << "MpcMip::InitRun, TriggerHelper not found" << endl;
      return ABORTRUN;
    }

  return 0;
}

int MpcMip::Init(PHCompositeNode *topNode)
{
  //  All Histoes registered with Fun4All...
  //  Using the TLA "EXM" to make unique...


  TString name;
  for (int ich=0; ich<576; ich++)
    {
      if ( mpcmap->getGridX(ich)<0 ) continue;
/*
      name = "hlopost_"; name += ich;
      hlopost[ich] = new TH1F(name,name,4096,-0.5,4095.5);
      HistoManager->registerHisto(name,hlopost[ich]);

      name = "hlopre_"; name += ich;
      hlopre[ich] = new TH1F(name,name,4096,-0.5,4095.5);
      HistoManager->registerHisto(name,hlopre[ich]);
*/

      name = "htdc_"; name += ich;
      htdc[ich] = new TH1F(name,name,4096,-0.5,4095.5);
      HistoManager->registerHisto(name.Data(),htdc[ich]);

      for (int itrig=0; itrig<6; itrig++)
        {
          name = "hlo"; name += itrig; name += "_"; name += ich;
          hlo[itrig][ich] = new TH1F(name,name,3196,-100.5,3095.5);
          HistoManager->registerHisto(name.Data(),hlo[itrig][ich]);

          name = "hhi"; name += itrig; name += "_"; name += ich;
          hhi[itrig][ich] = new TH1F(name,name,3196,-100.5,3095.5);
          HistoManager->registerHisto(name.Data(),hhi[itrig][ich]);
        }

      name = "hloadc_"; name += ich;
      hloadc[ich] = new TH1F(name,name,4096,-0.5,4095.5);
      HistoManager->registerHisto(name.Data(),hloadc[ich]);

      name = "hhiadc_"; name += ich;
      hhiadc[ich] = new TH1F(name,name,4096,-0.5,4095.5);
      HistoManager->registerHisto(name.Data(),hhiadc[ich]);

      for(int ibit=0; ibit<nbit; ibit++) {
	for(int irad=0; irad<nrad; irad++) {
	  name = "hhiadc_cut_"; name += ich; name+="_"; name+=irad; name+="_"; name+=ibit;
	  hhiadc_cut[ich][irad][ibit] = new TH1F(name,name,4096,-0.5,4095.5);
	  HistoManager->registerHisto(name.Data(),hhiadc_cut[ich][irad][ibit]);
	}
      }
    }

  htrig = new TH1F("htrig","htrig",6,-0.5,5.5);

  if ( treeflag )
    {
      ttreefile = new TFile(OutTreeFile.c_str(),"RECREATE");
      ttree = new TTree("miptree","MipStudyTree");
      ttree->SetMaxTreeSize(10000000000LL);
      //ttree->Branch("event",&event,"event/i");
      //ttree->Branch("strig",&scaledtrig,"strig/i");
      //ttree->Branch("zvtx",&zvtx,"zvtx/F");
      //ttree->Branch("bbcpmt",&pmt,"pmt/s");
      //ttree->Branch("bbcqn",&bbcqn,"bbcqn/F");
      //ttree->Branch("bbcqs",&bbcqs,"bbcqs/F");
      ttree->Branch("bbchit",&bbchit,"bbchit/F");
      ttree->Branch("mpchit",&mpchit,"mpchit/F");
      //ttree->Branch("bbcadc",&adc,"adc/s");
      //ttree->Branch("bbctdc0",&tdc0,"tdc0/s");
      //ttree->Branch("bbctdc1",&tdc1,"tdc1/s");
      ttree->Branch("arm",&arm,"arm/S");
      ttree->Branch("gridx",&gridx_value,"gridx_value/S");
      ttree->Branch("gridy",&gridy_value,"gridy_value/S");
      //ttree->Branch("loadc",&loadc_cor,"loadc_cor/F");
      ttree->Branch("hiadc",&hiadc_cor,"hiadc_cor/F");
      //ttree->Branch("mpctdc",&tdc,"tdc/I");
      ttree->Branch("dx",&dx,"dx/F");
      ttree->Branch("dy",&dy,"dy/F");
      ttree->Branch("hienergybitsum",&hienergybitsum,"hienergybitsum/S");
    }

  return 0;
}

int MpcMip::process_event(PHCompositeNode *topNode)
{
  // informational message...
  static int ncalls = 0;
  ncalls++;;
  if (ncalls % 1000 == 0 )
    {
      cout << "MpcMip Ncalls = " << ncalls << endl;
    }
 
  //  Get the data I need...
  mpcRawContainer *mpcraw = getClass<mpcRawContainer>(topNode, "MpcRaw");
  //PHGlobal *global = getClass<PHGlobal>(topNode, "PHGlobal");
  EventHeader *evtheader = getClass<EventHeader>(topNode, "EventHeader");
  BbcRaw *bbcraw = getClass<BbcRaw>(topNode, "BbcRaw");
  BbcOut *bbcout = getClass<BbcOut>(topNode, "BbcOut");
  
  if ( mpcraw==0 || evtheader==0 || bbcraw==0 || bbcout==0 )
    {
      cout << "MpcMip::process_event, mpcraw or eventheader or bbcraw or bbcout not found" << endl;
      cout << "\t" << (unsigned int)mpcraw
           << "\t" << (unsigned int)evtheader
           << "\t" << (unsigned int)bbcraw
           << "\t" << (unsigned int)bbcout
           << endl;
      return ABORTEVENT;
    }
  
  // cut out extreme vertices
  zvtx = bbcout->get_VertexPoint();
  if ( fabs(zvtx)>140. ) return EVENT_OK;
  
  const int NTRIG = 6;
  const int ALLTRIG = 0;
  const int MBIAS = 1;
  const int ALLMPC = 2;		// any mpc
  const int MPC4X4 = 3;
  const int MPC4X4_BBC = 4;
  const int MPC2X2 = 5;
  int trigger[NTRIG] = { 0 };
  int scaledown[NTRIG] = { 0 };

  trigger[ALLTRIG] = 1;
  // TriggerHelper doesn't know the MPC bits yet...
  TrigLvl1 *trigl1 = trighelp->get_trigLvl1();
  scaledtrig = trigl1->get_lvl1_trigscaled();

  // need to put in scaledowns for mpc triggers
  // but need trigger names for that
  scaledown[MBIAS] = trighelp->getLevel1Scaledown("BBCLL1(>0 tubes)");

  if ( (scaledtrig&0x4U) != 0 )
    {
      trigger[MBIAS] = 1;
      htrig->Fill(MBIAS,scaledown[MBIAS]+1);
    }
  if ( (scaledtrig&0x40U) != 0 )
    {
      trigger[MPC4X4] = 1;
      htrig->Fill(MPC4X4,scaledown[MPC4X4]+1);
    }
  if ( (scaledtrig&0x1000000U) != 0 )
    {
      trigger[MPC4X4_BBC] = 1;
      htrig->Fill(MBIAS,scaledown[MPC4X4_BBC]+1);
    }
  if ( (scaledtrig&0x80U) != 0 )
    {
      trigger[MPC2X2] = 1;
      htrig->Fill(MBIAS,scaledown[MPC2X2]+1);
    }
 
  //trigger[MBIAS] = trighelp->trigScaled("BBCLL1(>0 tubes)") ? 1 : 0;
  //trigger[MPC4X4A] = trighelp->trigRaw("MPCS 4x4a") ? 1 : 0;
  //trigger[MPC4X4C] = trighelp->trigRaw("MPCS 4x4c") ? 1 : 0;
  //trigger[MPC2x2] = trighelp->trigRaw("MPCS2x2") ? 1 : 0;
  
  if ( trigger[MPC4X4]==1 || trigger[MPC2X2]==1 || trigger[MPC4X4_BBC] ) trigger[ALLMPC] = 1;
  
  //cout << evtheader->get_EvtSequence() << "\t" << mpcraw->size() << endl;
  //cout << trigger[MBIAS] << "\t" << trigger[MPC4X4A] << "\t" << trigger[MPC4X4B] << endl;
  
  //const float adcgain = 1.0;
  //float mpcenergy = 0.;
  
  mpccalib = MpcCalib::instance();
  post_amu = mpcraw->get_post_amu();
  pre_amu = mpcraw->get_pre_amu();
  //int tdc_amu = mpcraw->get_tdc_amu();
  float lo_energy[2][18][18] = {{{0}}};
  float hi_energy[2][18][18] = {{{0}}};
  
  event = evtheader->get_EvtSequence();
  
  int nmpchit[2]={0};
  for (unsigned int ich = 0; ich < mpcraw->size(); ich++)
    {

      mpcRawContent *tower = mpcraw->getTower(ich); 

      int fee576ch = tower->get_ch();
      int tdc = tower->get_tdc();
      int xpos = mpcmap->getGridX( fee576ch );
      int ypos = mpcmap->getGridY( fee576ch );

      arm = 0;
      if ( fee576ch > 287 ) arm = 1;

      // skip channels that are supposed to be empty

      if ( xpos<0 ) continue;
      if ( ypos<0 ) continue;
 
      float loped = mpccalib->get_ped_lgpost(fee576ch,post_amu) - mpccalib->get_ped_lgpre(fee576ch,pre_amu);
      float hiped = mpccalib->get_ped_hgpost(fee576ch,post_amu) - mpccalib->get_ped_hgpre(fee576ch,pre_amu);

      lo_energy[arm][xpos][ypos]=( tower->get_lopost() - tower->get_lopre() - loped );
      hi_energy[arm][xpos][ypos]=( tower->get_hipost() - tower->get_hipre() - hiped );

      if ( hi_energy[arm][xpos][ypos] > HIT_TOWER_MIN_HIADC )
	{
	  nmpchit[arm]++;
	}

      // Fill histograms for hot/warm calibration
      if ( tdc<2100 ) htdc[fee576ch]->Fill(tdc);

      for (int itrig=0; itrig<NTRIG; itrig++)
        {
          if (trigger[itrig]==0) continue;

          hlo[itrig][fee576ch]->Fill(lo_energy[arm][xpos][ypos]);
          hhi[itrig][fee576ch]->Fill(hi_energy[arm][xpos][ypos]);

        }
    }
 
  //if ( trigger[MBIAS]==0 ) return EVENT_OK;
  
  for (unsigned int ich = 0; ich < mpcraw->size(); ich++)
    {
      
      mpcRawContent *tower = mpcraw->getTower(ich);  
      int fee576ch = tower->get_ch();
      arm = 0;
      if ( fee576ch>287 ) arm = 1;
      float lor2_min=100000000;
            
      gridx_value = mpcmap->getGridX( fee576ch );
      gridy_value = mpcmap->getGridY( fee576ch );
      tdc=0;
      hiadc_cor=0;
      loadc_cor=0;
      
      npmt_hit=0;
      pmt=0;
      adc=0;
      tdc0=0;
      tdc1=0;
      bbcqn=0;
      bbcqs=0;
      bbchit=0;
      mpchit=0;

      hienergybitsum=0;
      
      // skip channels that are supposed to be empty
      if ( gridx_value<0 ) continue;
      
      //Hi Energy cut
      if(hi_energy[arm][gridx_value][gridy_value]>=HIT_TOWER_MIN_HIADC)
	{

          // find number of hit towers around this tower
	  int hienergybit[9]={0};
	  for(int i=0; i<=2 ;i++)
	    {
	      for(int j=0; j<=2;j++)
		{
		  if(hi_energy[arm][gridx_value-1+j][gridy_value-1+i]>0)
		    {
		      hienergybit[3*i+j]=1;
		      //crystal map
		      //  7 8 9 
		      //  4 5 6
		      //  1 2 3
		    }
		}
	    }

	  for(int i=0; i<=8;i++)
	    {	  
	      hienergybitsum=hienergybitsum+hienergybit[i];
	    }

	  if(hienergybitsum<=4)
	    {       
	      short npmt =bbcraw->get_npmt();
	      int hipost = tower->get_hipost();
	      int hipre = tower->get_hipre();
	      int lopost = tower->get_lopost();
	      int lopre = tower->get_lopre();

	      float hipre_cor =hipre-mpccalib->get_ped_hgpre(fee576ch,pre_amu);
	      float hipost_cor =hipost-mpccalib->get_ped_hgpost(fee576ch,post_amu);
	      float hilo_ratio= mpccalib->get_hilo_ratio(fee576ch);
	      //float hilo_ratio=16.;
	      float lopre_cor =lopre-mpccalib->get_ped_lgpre(fee576ch,pre_amu);
	      float lopost_cor =lopost-mpccalib->get_ped_lgpost(fee576ch,post_amu);

	      hiadc_cor = (hipost_cor-hipre_cor)/hilo_ratio;
	      
	      hhiadc[fee576ch]->Fill( hiadc_cor );
	      loadc_cor = lopost_cor-lopre_cor;
	      hloadc[fee576ch]->Fill( loadc_cor );
	      tdc = tower->get_tdc();
	      
              // find dx, dy to nearest hit pmt in bbc (in projection to bbc)
	      int nbbchit[2]={0};
	      for(short ipmt=0;ipmt<npmt;ipmt++)
		{ 
                  // skip empty bbc hits
		  if (bbcraw->get_Tdc0(ipmt)>2700 && bbcraw->get_Tdc1(ipmt)>2700) continue;

		  int bbcarm=ipmt/64;
		  nbbchit[bbcarm]++;
                  npmt_hit++;

		  if(bbcarm==0) { bbcqs+=bbcraw->get_Adc(ipmt); }
		  else if ( bbcarm==1 ) { bbcqn+=bbcraw->get_Adc(ipmt); }

		  const int x=0;
		  const int y=1;
		  const int z=2;
		  float r2 = -9999.;
		  float rbbc_vec[3]={0.};
		  float bbcgeo[3]={0.};
		  float mpcgeo[3]={0.};
		  float vertexgeo[3]={0.};
		  
		  vertexgeo[x]=0.;
		  vertexgeo[y]=0.;
		  vertexgeo[z]=bbcout->get_VertexPoint();
		  bbcgeo[x]= bbcgeom->getX(ipmt)*MM2CM;	// bbc position is given in mm
		  bbcgeo[y]= bbcgeom->getY(ipmt)*MM2CM;
		  bbcgeo[z]= bbcgeom->getZ(ipmt)*MM2CM;
		  mpcgeo[x]= mpcmap->getX(fee576ch);
		  mpcgeo[y]= mpcmap->getY(fee576ch);  
		  mpcgeo[z]= mpcmap->getZ(fee576ch);
		  
		  // only pair towers and tubes from the same arm
		  if ( (mpcgeo[z] > 0 && bbcgeo[z] < 0) || (mpcgeo[z] < 0 && bbcgeo[z] > 0) ) continue; 
		    

                  for (int index=0;index<2;index++)
                    {
                      rbbc_vec[index]=( vertexgeo[index]+((bbcgeo[z]-vertexgeo[z])/(mpcgeo[z]-vertexgeo[z]))*((mpcgeo[index]- vertexgeo[index])));
                    }

                  float temp_dx=(bbcgeo[x]-rbbc_vec[x]);
	          float temp_dy=(bbcgeo[y]-rbbc_vec[y]);
                  r2 = (temp_dx*temp_dx+temp_dy*temp_dy);
			  
                  if (r2<lor2_min)  
                    {
                      lor2_min = r2;
                      dx = temp_dx;
                      dy = temp_dy;
                      pmt = bbcraw->get_Pmt(ipmt);
		      adc = bbcraw->get_Adc(ipmt);
		      tdc0 = bbcraw->get_Tdc0(ipmt);
		      tdc1 = bbcraw->get_Tdc1(ipmt);
		    }
		} // loop over bbc pmt's

              if ( treeflag && sqrt(lor2_min) < DR_MAXIMUM_CUT )
                {
		  mpchit=nmpchit[arm];
		  bbchit=nbbchit[arm];
                  ttree->Fill();
                }

	      double closest_radius=sqrt(lor2_min);
	      for(int irad=0; irad<nrad; irad++) 
		{ 
		  for(int ibit=0; ibit<nbit; ibit++)
		    {
		      if( lo_rad_cut[irad] <= closest_radius && closest_radius <= hi_rad_cut[irad] && lo_bit_cut[ibit] <= hienergybitsum && hienergybitsum <= hi_bit_cut[ibit] ) 
			{
			  hhiadc_cut[fee576ch][irad][ibit]->Fill(hiadc_cor);
			}
		    }
		}
           }	// hienergybitsum cut
       }
   }

  //any other return code might lead to aborting the event or analysis for everyone
  return EVENT_OK;
}

int MpcMip::End(PHCompositeNode *topNode)
{
  if ( treeflag )
    {
      ttreefile->Write(OutTreeFile.c_str());
      ttreefile->Close();
    }
  HistoManager->dumpHistos(OutHistFile.c_str());

  return 0;
}

