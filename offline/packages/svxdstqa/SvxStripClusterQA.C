#include <Bbc.hh>
#include <BbcOut.h>
#include <CglTrack.h>
#include <CrkRing.h>
#include <DchTrack.h>
#include <emcClusterContainer.h>
#include <emcClusterContent.h>
#include <ErtOut.h>
#include <EventHeader.h>
#include <Fun4AllReturnCodes.h>
#include <Fun4AllServer.h>
#include <PHCentralTrack.h>
#include <PHCompositeNode.h>
#include <PHGlobal.h>
#include <PHIODataNode.h>
#include <PHNodeIterator.h>
#include <PHPoint.h>
#include <PHTrackOut.h>
#include <RunHeader.h>
#include <TriggerHelper.h>
#include <TrigRunLvl1.h>
#include <VtxOut.h>

#include <phool.h>
#include <getClass.h>
#include <recoConsts.h>
#include <gsl/gsl_math.h>

#include "SvxStripClusterQA.h"

#include <SvxPriVertexSeedFinder.h>
#include <SvxPrimVertexFinder.h>
#include <SvxClusterList.h>
#include <SvxCluster.h>
#include <SvxSegmentList.h>
#include <SvxSegment.h>
#include <SvxBeamCenterPar.h>
#include <SvxCentralTrack.h>
#include <SvxCentralTrackReco.h>
#include <SvxCentralTrackList.h>
#include <SvxClusterInfo.h>
#include <SvxRawhit.h>
#include <SvxRawhitList.h>
#include <SvxRawhitCluster.h>
#include <SvxRawhitClusterList.h>
#include <SvxPixelHotDeadMap.h>
#include <SvxDeadMap.h>

#include "TFile.h"
#include "TH1F.h"
#include "TH2F.h"
#include "TF1.h"
#include "TGraphErrors.h"
#include "TCanvas.h"
#include "TTree.h"
#include "TROOT.h"
#include "TStyle.h"
#include "TProfile.h"
#include "TMath.h"
#include "TSystem.h"

#include <iostream>
#include <iomanip>
#include <map>
#include <string>
#include <algorithm>
#include <vector>
#include <cstdlib>
#include <cmath>

using namespace std;
using namespace findNode;

//--------------------------------------------------------------------------------

SvxStripClusterQA::SvxStripClusterQA(string filename):
  run(NULL),
  svxcluslist(NULL),
  svxrawhitlist(NULL),
  m_pixelhotdead(NULL),
  m_striphotdead(NULL)

{
  ThisName = "SvxStripClusterQA";
  outfilename = filename;

  init_variables();

  hstOutFile = NULL;
}

//--------------------------------------------------------------------------------

SvxStripClusterQA::~SvxStripClusterQA() 
{
  if(hstOutFile!=NULL) {delete hstOutFile; hstOutFile=NULL; }
}


int SvxStripClusterQA::Init(PHCompositeNode *topNode)    //called whenever datafrom a new run is encountered
{
  if(verbosity>0) cout << "Init:SvxStripClusterQA" << endl;

  init_histo();

  return 0;
}

//--------------------------------------------------------------------------------
int SvxStripClusterQA::InitRun(PHCompositeNode *topNode) //called once at startup
{
  if(verbosity)
    {
    }

  init_nodes(topNode);
 
  return EVENT_OK;
}

int SvxStripClusterQA::process_event(PHCompositeNode *topNode) //called for every event
{
  // check objects exist
  if( run              ==NULL ||
      svxcluslist      ==NULL ||
      svxtracks        ==NULL ||
      svxrawhitlist    ==NULL ||
      m_striphotdead   ==NULL 
  ) {
      cerr<<"Error SvxStripClusterQA::process_event"<<endl;
      if(run              ==NULL) cout<<" No Object : RunHeader"<<flush;
      if(svxcluslist      ==NULL) cout<<" No Object : SvxClusterList"<<flush;
      if(svxtracks        ==NULL) cout<<" No Object : SvxSegmentList"<<flush;
      if(svxrawhitlist    ==NULL) cout<<" No Object : SvxRawhitList"<<flush;
      //if(svxrawhit2cluster==NULL) cout<<" No Object : SvxRawhitClusterList"<<flush;
      if(m_striphotdead   ==NULL) cout<<" No Object : SvxDeadMap"<<flush;

    return EVENT_OK;
  }




  init_variables();

  fill_histo();

  return EVENT_OK;
}

//-------------------------------------------------------------------------------

int SvxStripClusterQA::End(PHCompositeNode *topNode) // Last call before you quit
{ 
  if(verbosity>0)cout << PHWHERE << "Writing out histograms..." << endl;
  if(hstOutFile!=NULL){
    hstOutFile->cd(); //Save under these object
    hstOutFile->Write();
    hstOutFile->Close();
  }
  if(verbosity>0) cout << PHWHERE << "   done." << endl;
  return EVENT_OK;
}


bool SvxStripClusterQA::init_histo()
{
  
  hstOutFile = new TFile(outfilename.c_str(),"RECREATE");
  hstOutFile->cd();
  cout << PHWHERE << "Output histogram file opened: " << outfilename << endl;
  
  //define hist

  //rawhit flag
  h1_rawhitflag[0] = new TH1F("h1_rawhitflag_2", "raw hit flag for barrel 2", 10, -5-0.5, 5-0.5);
  h1_rawhitflag[1] = new TH1F("h1_rawhitflag_3", "raw hit flag for barrel 3", 10, -5-0.5, 5-0.5);

  h1_rawhitflagdiff[0] = new TH1F("h1_rawhitflagdiff_2", "raw hit flag diff for barrel 2", 10, -5-0.5, 5-0.5);
  h1_rawhitflagdiff[1] = new TH1F("h1_rawhitflagdiff_3", "raw hit flag diff for barrel 3", 10, -5-0.5, 5-0.5);

  //cluster z-phi distribution
  h2_cluster_zphi[0] = new TH2F("h2_cluster_zphi_2", "cluster distribution of barrel 2", 400, -20, 20, 700, -2, 5);
  h2_cluster_zphi[1] = new TH2F("h2_cluster_zphi_3", "cluster distribution of barrel 3", 400, -20, 20, 700, -2, 5);

  for(int i = 0; i < 2; i++)
    {
      h2_cluster_zphi[i]->GetXaxis()->SetTitle("z [cm]");
      h2_cluster_zphi[i]->GetYaxis()->SetTitle("#phi [rad]");
    }

  //cluster adc distritubion
  h2_adc_barrel[0][0] = new TH2F("h2_adc_barrel2X", "adc X of barrel 2", 80, 0, 80, 300, 0, 300);
  h2_adc_barrel[0][1] = new TH2F("h2_adc_barrel2U", "adc U of barrel 2", 80, 0, 80, 300, 0, 300);

  h2_adc_barrel[1][0] = new TH2F("h2_adc_barrel3X", "adc X of barrel 3", 144, 0, 144, 300, 0, 300);
  h2_adc_barrel[1][1] = new TH2F("h2_adc_barrel3U", "adc U of barrel 3", 144, 0, 144, 300, 0, 300);

  //cluster size distritubion
  h2_size_barrel[0][0] = new TH2F("h2_size_barrel2X", "size X of barrel 2", 80, 0, 80, 30, 0, 30);
  h2_size_barrel[0][1] = new TH2F("h2_size_barrel2U", "size U of barrel 2", 80, 0, 80, 30, 0, 30);

  h2_size_barrel[1][0] = new TH2F("h2_size_barrel3X", "size X of barrel 3", 144, 0, 144, 30, 0, 30);
  h2_size_barrel[1][1] = new TH2F("h2_size_barrel3U", "size U of barrel 3", 144, 0, 144, 30, 0, 30);

  //cluster with hot size z-phi distritubion
  h2_cluster_hotsize_zphi_barrel[0][0] = new TH2F("h2_cluster_hotsize_zphi_barrel2X", "cluster with hot size X of barrel 2", 400, -20, 20, 700, -2, 5);
  h2_cluster_hotsize_zphi_barrel[0][1] = new TH2F("h2_cluster_hotsize_zphi_barrel2U", "cluster with hot size U of barrel 2", 400, -20, 20, 700, -2, 5);

  h2_cluster_hotsize_zphi_barrel[1][0] = new TH2F("h2_cluster_hotsize_zphi_barrel3X", "cluster with hot size X of barrel 3", 400, -20, 20, 700, -2, 5);
  h2_cluster_hotsize_zphi_barrel[1][1] = new TH2F("h2_cluster_hotsize_zphi_barrel3U", "cluster with hot size U of barrel 3", 400, -20, 20, 700, -2, 5);

  return true;
}


bool SvxStripClusterQA::init_nodes(PHCompositeNode *topNode)
{

  // event_header = getClass<EventHeader>(topNode,"EventHeader");
  run = findNode::getClass<RunHeader>(topNode,"RunHeader");
  if(!run) 
    {
      cerr <<  PHWHERE  << " ERROR: Can't find RunHeader " << endl; 
      return false;
    } 
    
  m_pixelhotdead = findNode::getClass<SvxPixelHotDeadMap>(topNode, "SvxPixelHotDeadMap");
  if (!m_pixelhotdead) 
    {
      cerr << PHWHERE<< "ERROR: Can't find SvxPixelHotDeadMap. " << endl; 
      return false;
  }

  m_striphotdead = findNode::getClass<SvxDeadMap>(topNode, "SvxStripHotDeadMap");
  if(!m_striphotdead) 
    {
      cerr << PHWHERE<< "ERROR: Can't find SvxStripHotDeadMap. " << endl;
      return false;
    }


  //rawhit
  svxrawhitlist = findNode::getClass<SvxRawhitList>(topNode, "SvxRawhitList");
  if(!svxrawhitlist) 
    {
      cerr << PHWHERE<< "ERROR: Can't find SvxRawhitList. " << endl;
      return false;
    }

  //cluster
  svxcluslist = findNode::getClass<SvxClusterList>(topNode, "SvxClusterList");
  if(!svxcluslist) 
    {
      cerr << PHWHERE<< "ERROR: Can't find SvxClusterList. " << endl;
      return false;
    }

  //track
  svxtracks = findNode::getClass<SvxSegmentList>(topNode, "SvxSegmentList");
  if(!svxtracks) 
    {
      cerr << PHWHERE<< "ERROR: Can't find SvxSegmentList. " << endl;
      return false;
    }

  return true;
}

void SvxStripClusterQA::init_variables()
{

  charge=0;
  for(int i=0;i<4;i++) phi[i]=-9999; 
  for(int i=0;i<4;i++) phib[i]=-9999;
  nsvxtracks=0;
  runnumber=-1;

  for(int i=0;i<3;i++) m_vtxout[i]=-9999;//data from vtxout node
  for(int i=0;i<3;i++) svxseed[i]=-9999;//data from vtxout node
  for(int i=0;i<3;i++) svxprim[i]=-9999;//data from vtxout node
  
  if(verbosity>0)  cout << "init variables" << endl;
  return;
}


void SvxStripClusterQA::fill_histo()
{
  //zvtx 
  runnumber = run->get_RunNumber();
  if(verbosity>0)  cout << "filling histo" << endl;

  nsvxrawhit = svxrawhitlist->get_nRawhits();
  nsvxcluster = svxcluslist->get_nClusters();
  nsvxtracks = svxtracks->get_nSegments();

  //
  // RawHits
  //
  
  for(int i = 0; i < nsvxrawhit; i++)
    {
      int layer = svxrawhitlist->get_Rawhit(i)->get_layer();
      int ladder = svxrawhitlist->get_Rawhit(i)->get_ladder();
      int sensor = svxrawhitlist->get_Rawhit(i)->get_sensor();
      int channel = svxrawhitlist->get_Rawhit(i)->get_channel();

      int flag = svxrawhitlist->get_Rawhit(i)->get_HotDeadFlag();
      int status = -9999;

      if(layer == 2 || layer == 3) //strip
	{
	  int SS      = svxrawhitlist->get_Rawhit(i)->get_sensorSection();
	  int readout = svxrawhitlist->get_Rawhit(i)->get_sensorReadout();

	  status = m_striphotdead->channelStatus(layer-2, ladder, sensor, SS, readout, channel, 0);

	  int diff = flag - status;

	  h1_rawhitflag[layer-2]->Fill(flag);
	  h1_rawhitflagdiff[layer-2]->Fill(diff);
	}
    }

  //
  // Segment (StandAloneTracking)
  //
  
  if(verbosity>0) cout << "SvxStripClusterQA::fill_histo() " << endl;
  
  for(int i = 0; i < nsvxcluster; i++)
    {
      float x = -9999;
      float y = -9999;
      float z = -9999;
      int layer = -9999;
      int ladder = -9999;
      int sensor = -9999;

      float phi = -9999;
      float eta = -9999;

      x = svxcluslist->get_Cluster(i)->get_xyz_global(0);
      y = svxcluslist->get_Cluster(i)->get_xyz_global(1);
      z = svxcluslist->get_Cluster(i)->get_xyz_global(2);
      layer = svxcluslist->get_Cluster(i)->get_layer();
      ladder = svxcluslist->get_Cluster(i)->get_ladder();
      sensor = svxcluslist->get_Cluster(i)->get_sensor();
      
      phi = atan2(y,x);
      if(phi <-1*acos(-1.0)/2)
	{
	  phi = phi+2*acos(-1.0);
	}
      
      float r = sqrt(x*x +y*y);
      //float theta = atan2(z,r);
      float theta = atan2(r,z);
      eta  = -log(tan(theta/2.0));
      if(fabs(eta)>100) cout<<"eta : "<<eta<<endl;

      if(layer == 2 || layer == 3)
	{
	  h2_cluster_zphi[layer-2]->Fill(z, phi);

	  for(int j = 0; j < 2; j++)
	    {
	      float adc = svxcluslist->get_Cluster(i)->get_adc(j);
	      float size = svxcluslist->get_Cluster(i)->get_xz_size(j);
	      
	      if(layer == 2)
		{
		  h2_adc_barrel[layer-2][j]->Fill(5 * ladder + sensor, adc);
		  h2_size_barrel[layer-2][j]->Fill(5 * ladder + sensor, size);
		}	
	      else if(layer == 3)
		{
		  h2_adc_barrel[layer-2][j]->Fill(6 * ladder + sensor, adc);
		  h2_size_barrel[layer-2][j]->Fill(6 * ladder + sensor, size);
		}

	      if(size >= 10)//cluster has hot size
		{
		  h2_cluster_hotsize_zphi_barrel[layer-2][j]->Fill(z, phi);
		}
	    }
	}          
    }

  if(verbosity>0) cout << "SvxStripClusterQA::fill_histo() End " << endl;
}


