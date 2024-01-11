//  GENERal PHENIX tools
#include <Fun4AllServer.h>
#include <getClass.h>
#include <PHCompositeNode.h>
#include <phool.h>
#include <Fun4AllHistoManager.h>
#include <Fun4AllReturnCodes.h>
#include <recoConsts.h>

//  Data classes I am using in analysis
#include <TriggerHelper.h>
#include <TrigLvl1.h>
#include <PHGlobal.h>
#include <EventHeader.h>
#include <mpcRawContainer.h>
#include <mpcRawContent.h>
#include <BbcOut.h>
#include <BbcRaw.h>
#include <Bbc.hh>
#include <MpcCalib.h>
//#include <MpcGeom.h>
#include <BbcGeo.hh>
#include <mpcClusterContainer.h>
#include <mpcClusterContent.h>
#include <mpcTowerContainer.h>
#include <mpcTowerContent.h>
#include <mpcPi0TTree.h>
#include <RunHeader.h>
#include <MpcMap.h>

//  Root histogram types
#include <TH1.h>
#include <TH2.h>
#include <TCanvas.h>
#include <TNtuple.h>
#include <TTree.h>
#include <TFile.h>
#include <TLorentzVector.h>
#include <TVector3.h>
#include <cmath>



using namespace std;
using namespace findNode;


mpcPi0TTree::mpcPi0TTree(const char* outfile,  const char* inDeadTowerList) : SubsysReco("MpcTriggerStudy Analyzer")
{
  //  Take the output file name as an argument
  OutFileName = outfile;
  DeadTowerList=inDeadTowerList;
  simulation_flag = 0;	// by default we are not running on sims
  return ;
}

mpcPi0TTree::~mpcPi0TTree()
{
}

int mpcPi0TTree::InitRun(PHCompositeNode *topNode)
{
  mpcmap = getClass<MpcMap>(topNode, "MpcMap");


  /* This section was used in debugging the swapping of towers with mpc map but is no longer needed
  int ix1o = mpcmap->getGridX(313); 
  float x1o = mpcmap->getX(313); 
  int ix2o = mpcmap->getGridX(312); 
  float x2o = mpcmap->getX(312); 
 
  cout << "------------------------------------- redownloading maps with text file -----------------------------------------------\n\n"; 
  //  mpcmap->Download_Maps(t_mapfile.Data()); 
 
  cout << "testing map data:\n"; 
  int ix1 = mpcmap->getGridX(313); 
  float x1 = mpcmap->getX(313); 
  int ix2 = mpcmap->getGridX(312); 
  float x2 = mpcmap->getX(312); 
 
 
  cout << "before: ch1,ix1,x1\t ch2,ix2,x2" << (int) 313 << ", " << ix1o << ", " << x1o << "\t" << (int) 312 << ", " << ix2o << ", " << x2o << endl << endl; 
  cout << "after: ch1,ix1,x1\t ch2,ix2,x2" << (int) 313 << ", " << ix1 << ", " << x1 << "\t" << (int) 312 << ", " << ix2 << ", " << x2 << endl << endl; 
  
  
   
 
 
  cout << "------------------------------------- maps have been downloaded -----------------------------------------------\n\n"; 
  */
  
  //END_MPC 



  for(int impc=0;impc<2;impc++)
    for(int iy=0;iy<18;iy++)
      for(int ix=0;ix<18;ix++)
	{tower[impc][iy][ix]=0;} 
  
  //it is easier to use the dead map after the ttree is processed and so I do not implement it here
    
    
  /*ifstream inFile;
  inFile.open(DeadTowerList.Data(),ios::in);
    if (inFile.is_open())
      {
      while (! inFile.eof() )
      {
	  //int count=0;
	  int arm, x,y;
 	  inFile>>arm >>x>>y;
	  tower[arm][y][x] = -1;
	  if(arm < 0 || arm > 1) {cout << "exiting wm\n"; break;}
	}
      inFile.close();
      for(int impc=0;impc<2;impc++)
	for(int iy=0;iy<18;iy++)
	for(int ix=0;ix<18;ix++)
	    {std::cout << mpcmap->getFeeCh(ix,iy,impc) << ", " << tower[impc][iy][ix] << endl;}
	    }
    else cout << "Unable to open file"; */
  
  recoConsts *rc = recoConsts::instance();
  rc->Print();
  
  if (simulation_flag==0)
    {
      trighelp = new TriggerHelper(topNode);
      if ( trighelp==0 )
        {
          cout << "mpcPi0TTree::InitRun, TriggerHelper not found" << endl;
          return ABORTRUN;
        }
    }
  
  return 0;
}

int mpcPi0TTree::Init(PHCompositeNode *topNode)
{

  pi0file = new TFile(OutFileName.c_str(),"RECREATE");

  //ttree contains information for cluster pairs
  //stree only contains info for single clusters...it is useful for debugging things like swapped towers :)

  ttree = new TTree("pi0_ttree","Pi0_study");
  ttree->Branch("event",&event,"event/i");
  ttree->Branch("cent",&cent,"cent/F");
  ttree->Branch("x1",&x1,"x1/F");
  ttree->Branch("y1",&y1,"y1/F");
  ttree->Branch("x2",&x2,"x2/F");
  ttree->Branch("y2",&y2,"y2/F");
  //  ttree->Branch("pi0_x",&pi0_x,"pi0_x/F"); 
  //ttree->Branch("pi0_y",&pi0_y,"pi0_y/F"); 
  ttree->Branch("pi0_z",&pi0_z,"pi0_z/F"); 


  ttree->Branch("disp",&disp,"disp/F");
  ttree->Branch("cdisp",&cdisp,"cdisp/F");
  ttree->Branch("ldisp",&ldisp,"ldisp/F");
  ttree->Branch("lcdisp",&lcdisp,"lcdisp/F");
  ttree->Branch("corrdispx1",&corrdispx1,"corrdispx1/F");
  ttree->Branch("corrdispy1",&corrdispy1,"corrdispy1/F");
  ttree->Branch("corrdispx2",&corrdispx2,"corrdispx2/F");
  ttree->Branch("corrdispy2",&corrdispy2,"corrdispy2/F");
  
  ttree->Branch("chi2",&chi2,"chi2/F");
  ttree->Branch("chi1",&chi1,"chi1/F");
  ttree->Branch("pi0_chi",&pi0_chi,"pi0_chi/F");
  


  ttree->Branch("pi0_px",&pi0_px,"pi0_px/F"); 
  ttree->Branch("pi0_py",&pi0_py,"pi0_py/F"); 
  ttree->Branch("pi0_pz",&pi0_pz,"pi0_pz/F"); 
  ttree->Branch("phi",&phi,"phi/F");
  ttree->Branch("theta",&theta,"theta/F");
  ttree->Branch("px1",&px1,"px1/F"); 
  ttree->Branch("py1",&py1,"py1/F"); 
  ttree->Branch("pz1",&pz1,"pz1/F"); 
  ttree->Branch("px2",&px2,"px2/F"); 
  ttree->Branch("py2",&py2,"py2/F"); 
  ttree->Branch("pz2",&pz2,"pz2/F"); 
  ttree->Branch("energy1",&energy1,"energy1/F");
  ttree->Branch("energy2",&energy2,"energy2/F"); 
  ttree->Branch("pi0_energy",&pi0_energy,"pi0_energy/F");
  ttree->Branch("mass",&mass,"mass/F");
  ttree->Branch("pt",&pt,"pt/F");
  ttree->Branch("arm1",&arm1,"arm1/i");
  ttree->Branch("fee1",&fee1,"fee1/i");
  ttree->Branch("fee2",&fee2,"fee2/i");
  ttree->Branch("ntow1",&ntow1,"ntow1/I");
  ttree->Branch("etow1",etow1,"etow1[ntow1]/F");
  ttree->Branch("ch1",ch1,"ch1[ntow1]/F");

  ttree->Branch("ntow2",&ntow2,"ntow2/I");
  ttree->Branch("etow2",etow2,"etow2[ntow2]/F");
  ttree->Branch("ch2",ch2,"ch2[ntow2]/F");
  //ttree->Branch("event",&event,"event/F");
  //  ttree->Branch("ixpos",&ixpos,"ixpos/i");
  //   ttree->Branch("iypos",&iypos,"iypos/i");
  
  
  
  
  stree = new TTree("stree","single cluster study");
  stree->Branch("event",&event,"event/i");
  stree->Branch("cent",&cent,"cent/F");
  stree->Branch("x1",&x1,"x1/F");
  stree->Branch("y1",&y1,"y1/F");
  stree->Branch("z1",&y1,"z1/F");
  stree->Branch("zvtx",&zvtx,"zvtx/F");



  stree->Branch("disp",&disp,"disp/F");
  stree->Branch("cdisp",&cdisp,"cdisp/F");
  stree->Branch("ldisp",&ldisp,"ldisp/F");
  stree->Branch("lcdisp",&lcdisp,"lcdisp/F");
  
  stree->Branch("chi1",&chi1,"chi1/F");

  stree->Branch("phi",&phi,"phi/F");
  stree->Branch("theta",&theta,"theta/F");
  stree->Branch("eta",&eta,"eta/F");
  
  stree->Branch("px1",&px1,"px1/F"); 
  stree->Branch("py1",&py1,"py1/F"); 
  stree->Branch("pz1",&pz1,"pz1/F"); 
  stree->Branch("energy1",&energy1,"energy1/F");
  stree->Branch("pt",&pt,"pt/F");
  stree->Branch("arm1",&arm1,"arm1/i");
  stree->Branch("fee1",&fee1,"fee1/i");
  stree->Branch("ntow1",&ntow1,"ntow1/I");
  stree->Branch("etow1",etow1,"etow1[ntow1]/F");
  stree->Branch("ch1",ch1,"ch1[ntow1]/F");

  stree->Branch("ntow_tot",&ntow_tot,"ntow_tot/I");
  stree->Branch("etow",etow,"etow[ntow_tot]/F");
  stree->Branch("chtow",chtow,"chtow[ntow_tot]/F");
  stree->Branch("ixtow",ixtow,"ixtow[ntow_tot]/I");
  stree->Branch("iytow",iytow,"iytow[ntow_tot]/I");


  //ttree->Branch("event",&event,"event/F");
  stree->Branch("ixpos",&ixpos,"ixpos/I");
  stree->Branch("iypos",&iypos,"iypos/I");
  
  
  

  return 0;
}

int mpcPi0TTree::process_event(PHCompositeNode *topNode)
{
  // use only minbias events
  if ( simulation_flag==0 && trighelp->IsEventMinBias()==false ) return EVENT_OK;

  // informational message...
  static int ncalls = 0;
  ncalls++;;
  if (ncalls % 1000 == 0 && verbosity)
    {
      cout << "mpcPi0TTree Ncalls = " << ncalls << endl;
    }
  

  //  Get the data I need...
  evtheader = getClass<EventHeader>(topNode, "EventHeader");
  runheader = getClass<RunHeader>(topNode, "RunHeader");
  mpcclus = findNode::getClass<mpcClusterContainer>(topNode,"mpcClusterContainer");
  global = findNode::getClass<PHGlobal>(topNode, "PHGlobal");
  //bbcout = findNode::getClass<BbcOut>(topNode, "BbcOut");
  mpcTowerContainer* mpctow = getClass<mpcTowerContainer>(topNode,"mpcTowerContainer");

  if (mpcclus==0 || evtheader==0 || global==0 || runheader==0)
    {
      cout << "mpcPi0TTree::process_event  mpcclus, evtheader, global, runheader not found" << endl;
      cout << "\t" << (unsigned int)mpcclus 
	   << "\t" << (unsigned int)evtheader 
	   << "\t" << (unsigned int)global
	   << "\t" << (unsigned int)runheader 
           << endl;
      
      return ABORTEVENT;
    }
  
  
  int nclus = mpcclus->size();
  //float zvertex = bbcout->get_VertexPoint();
  float zvertex = global->getBbcZVertex();
  zvtx = zvertex;
  event =evtheader->get_EvtSequence();
  run = runheader->get_RunNumber();
  cent = global->getCentrality();

  //cout <<"Beginning of event with clusters of size: " << nclus << endl;
  //for (int iclus1=0; iclus1<nclus; iclus1++)
  // cout<<"run number is "<<run<<endl;


  bool isgood = 0;


  for (int iclus1=0; iclus1<nclus-1; iclus1++)
    {
      mpcClusterContent *clus1 = mpcclus->getCluster(iclus1);
      
      ixpos=clus1->ixpos();
      iypos=clus1->iypos();
      //so ixpos & iypos were switched so lets take that into acount
      
      chi1 = clus1->chi2core();
      
      energy1 = clus1->ecore();      // use 3x3 sum for energy
      
      if(energy1 < 2) continue;
      if(chi1 > 3) continue;
      
      
      arm1 = clus1->arm();
      // cout << "Arm1: " << arm1 << endl;
      
      int ixpos1=clus1->ixpos();
      int iypos1=clus1->iypos();
      fee1 = mpcmap->getFeeCh(ixpos1,iypos1,arm1);

      //      if(fee1 == 313 || fee1 == 312 || fee1 == 310) isgood = 1; //used for stree only
	    

      int mult = clus1->multiplicity();
      ntow1 = mult;
      for(int itow=0;itow<mult;itow++){
	int ich=clus1->towerid(itow);
	ch1[itow] = (short)ich;
	float parte = clus1->partesum(itow);
	etow1[itow] = parte;
      }
        
      driver1 = mpcmap->getDriver(fee1);
        
      //    if (tower[arm1][iypos1][ixpos1]==-1) continue;
      

      x1 = clus1->x();
      y1 = clus1->y();
      z1 = clus1->z()-zvertex;
      pi0_z = z1;
      
      TVector3 T3v1(x1,y1,z1);
      
      px1=energy1*T3v1(0)/T3v1.Mag();
      py1=energy1*T3v1(1)/T3v1.Mag();
      pz1=energy1*T3v1(2)/T3v1.Mag();
      
      TLorentzVector T4v1(px1,py1,pz1,energy1);
      
      corrdispx1 = clus1->corrdispz();
      corrdispy1 = clus1->corrdispy();
      dispx1 = clus1->dispz();
      dispy1 = clus1->dispy();
      logcorrdispx1 = clus1->corrlogdispx();
      logcorrdispy1 = clus1->corrlogdispy();
      logdispx1 = clus1->logdispx();
      logdispy1 = clus1->logdispy();
      
      float disp1 = max(dispx1,dispy1);
      float cdisp1 = max(corrdispx1,corrdispy1);
      float ldisp1 = max(logdispx1,logdispy1);
      float lcdisp1 = max(logcorrdispx1,logcorrdispy1);
      
      
      
      //for (int iclus2=0; iclus2<0; iclus2++) 
      for (int iclus2=iclus1+1; iclus2<nclus; iclus2++)
	{

	  mpcClusterContent *clus2 = mpcclus->getCluster(iclus2);
	  fee2 = clus2->towerid(0);
	  //if(fee1 != 312 && fee1 != 313 && fee2 != 312 && fee2 != 312) continue;
	  energy2 = clus2->ecore();      // use 3x3 sum for energy
	  if(energy2 < 2) continue;
	  float alpha=fabs((energy1-energy2)/(energy1+energy2));
	  if(alpha>=0.6) continue;
	  chi2 = clus2->chi2core();
	  if(chi2 > 3) continue;
	  if( (energy1 + energy2) < 7 ) continue;

	  driver2 = mpcmap->getDriver(fee2);

	  int mult = clus2->multiplicity();
	  ntow2 = mult;
	  for(int itow=0;itow<mult;itow++){
	    int ich=clus2->towerid(itow);
	    ch2[itow] = (short)ich;
	    float parte = clus2->partesum(itow);
	    etow2[itow] = parte;
	  }

  
	  //	  if (tower[arm2][ypos2][xpos2]==-1) continue;
	  //	  if(arm1 != arm2) continue;
	  
	  x2 = clus2->x();
	  y2 = clus2->y();
	  z2 = clus2->z()-zvertex;
	  
	  TVector3 T3v2(x2,y2,z2);
	  
	  px2=energy2*T3v2(0)/T3v2.Mag();
	  py2=energy2*T3v2(1)/T3v2.Mag();
	  pz2=energy2*T3v2(2)/T3v2.Mag();
	  
	  TLorentzVector T4v2(px2,py2,pz2,energy2);
	
	  
	  corrdispx2 = clus2->corrdispz();
          corrdispy2 = clus2->corrdispy();
          dispx2 = clus2->dispz();
          dispy2 = clus2->dispy();

          logcorrdispx2 = clus2->corrlogdispx();
          logcorrdispy2 = clus2->corrlogdispy();
          logdispx2 = clus2->logdispz();
          logdispy2 = clus2->logdispy();
         

	  float disp2 = max(dispx2,dispy2);
	  float cdisp2 = max(corrdispx2,corrdispy2);
	  float ldisp2 = max(logdispx2,logdispy2);
	  float lcdisp2 = max(logcorrdispx2,logcorrdispy2);
          disp = max(disp1,disp2);
          ldisp = max(ldisp1,ldisp2);
          cdisp = max(cdisp1,cdisp2);
          lcdisp = max(lcdisp1,lcdisp2);
	  
	  pi0_chi = max(chi1,chi2);


	  if(pi0_chi > 3) continue;
	  	  
	  pi0_px=px1+px2; 
	  pi0_py=py1+py2; 
	  pi0_pz=pz1+pz2; 
	  phi = atan2(pi0_py,pi0_px);
	  pi0_energy=energy1+energy2;

	  if (pi0_energy<7) continue;
	  
	  pt=sqrt((pi0_px)*(pi0_px)+(pi0_py)*(pi0_py));
	  if(pt < 0.5) continue;
	  theta = acos(pi0_pz/sqrt(pt*pt+pi0_pz*pi0_pz));

          float mass2=(T4v1+T4v2)*(T4v1+T4v2);
	  if(mass2 <= 0) continue;
	  mass = sqrt(mass2);

	  //  mass=sqrt(T4v1*T4v2);
	  if(mass > 1) continue;


	  ttree->Fill();
	}
    }

  if(isgood){
  
    int n_itow = 0;
    for (unsigned int itow=0; itow<mpctow->size(); itow++){
      mpcTowerContent *tow = mpctow->getTower(itow);
      int ch = tow->get_ch();
      if(ch < 288) continue; //only worry about north for now
      
      etow[n_itow] = tow->get_energy();
      ixtow[n_itow] = mpcmap->getGridX(ch);
      iytow[n_itow] = mpcmap->getGridY(ch);
      chtow[n_itow] = ch;
      
      n_itow++;
      ntow_tot = n_itow;
    }
    stree->Fill();
  }



  return EVENT_OK;
}



int mpcPi0TTree::End(PHCompositeNode *topNode)
{
  pi0file->Write();
  pi0file->Close();
  return 0;
}
