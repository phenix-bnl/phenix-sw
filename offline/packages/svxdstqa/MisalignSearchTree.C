#include "MisalignSearchTree.h"
#include "SvxQADefs.h"

#include <phool.h>
#include <PHCompositeNode.h>
#include <Fun4AllServer.h>
#include <Fun4AllHistoManager.h>
#include <Fun4AllReturnCodes.h>
#include <getClass.h>
#include <TrigLvl1.h>

#include <EventHeader.h>
#include <RunHeader.h>
#include <PreviousEvent.h>
#include <SvxRawhitList.h>
#include <SvxRawhit.h>
#include <SvxClusterList.h>
#include <SvxCluster.h>
#include <svxAddress.hh>
#include <VtxOut.h>
#include <PHPoint.h>
#include <SvxStripThreshold.h>
#include <BbcOut.h>
#include <Bbc.hh>
#include <SvxQAEventSelection.h>

#include <TFile.h>
#include <TTree.h>

#include <iostream>
#include <cmath>

using namespace std;
using namespace findNode;

MisalignSearchTree::MisalignSearchTree():
  SubsysReco("MISALIGNSEARCHTREE"),
  d_runheader(NULL),
  d_eventhead(NULL),
  d_svxraw(NULL),
	d_bbc(NULL),
	d_trigl1(NULL)
{
	reset_variables();
}


int MisalignSearchTree::Init(PHCompositeNode *topNode)
{
	outfile = new TFile(outname.c_str(),"recreate");
	outfile->cd();

	tree = new TTree("tree","Tree of pixel chip hits");
	tree->Branch("event",&event,"event/I");
	tree->Branch("bbcq",&bbcq,"bbcq/F");
	tree->Branch("trigword",&trigword,"trigword/i");
	tree->Branch("hits",&hits,"hits[30][16]/b");

  return EVENT_OK;
}

//==============================================================
int MisalignSearchTree::InitRun(PHCompositeNode *topNode)
{

  d_runheader = findNode::getClass<RunHeader>(topNode, "RunHeader");
  if (d_runheader == NULL)
  {
    if (verbosity > 0)
      cout << PHWHERE << "Can't find RunHeader. " << endl;
    return ABORTRUN;
  }

  return EVENT_OK;
}
//==============================================================
int MisalignSearchTree::process_event(PHCompositeNode *topNode)
{
	reset_variables();

  GetNodes(topNode);

  if ( d_trigl1 != 0 )
  {
    trigword = d_trigl1->get_lvl1_trigscaled();
	}

  if(d_bbc != 0)
	  bbcq = d_bbc->get_ChargeSum(Bbc::North) + d_bbc->get_ChargeSum(Bbc::South);

  event = d_eventhead->get_EvtSequence();

  //raw hit info
  int nrawhit = (d_svxraw != NULL) ? d_svxraw->get_nRawhits() : 0;

  for (int ihit = 0; ihit < nrawhit; ihit++)
  {
    SvxRawhit *rhit = d_svxraw->get_Rawhit(ihit);
    int r_section = rhit->get_svxSection();
    int r_layer   = rhit->get_layer();
    int r_ladder  = rhit->get_ladder();
    int r_sensor  = rhit->get_sensor();

    if(r_layer > 1) continue;

    int chip_no; //chip number

    if (r_section == 0) // pixel or stripixel
    {
      int pixelRoc = rhit->get_pixelROC();
      int sensor_chip = pixelRoc;
      if (r_sensor == 1 || r_sensor == 3)
          sensor_chip -= 4;

      chip_no  =  r_sensor * 4 + sensor_chip;
      int ladder = r_layer>0 ? r_ladder+10 : r_ladder;
      hits[ladder][chip_no]++;
    }
  }

	tree->Fill();

  return EVENT_OK;
}
//==============================================================

//----Classes needed----------------------------------------

int MisalignSearchTree::GetNodes(PHCompositeNode *topNode)
{

    //-------------------EventHeader----------------------------------
    d_eventhead = findNode::getClass<EventHeader>(topNode, "EventHeader");
    if (d_eventhead == NULL)
    {
        cerr << PHWHERE << " EventHeader node not found." << endl;
        return DISCARDEVENT;
    }
    //------------------------------------------------------------------

    //----------------SvxRawhitList-------------------------
    d_svxraw = findNode::getClass<SvxRawhitList>(topNode, "SvxRawhitList");
    if (d_svxraw == NULL)
    {
        cerr << "SvxRawhit node not found." << endl;
        return DISCARDEVENT;
    }
    //------------------------------------------------------

    //-------------------BbcOut----------------------------------------
    d_bbc = NULL;
    d_bbc = findNode::getClass<BbcOut>(topNode, "BbcOut");
    if (d_bbc == NULL)
    {
        cerr << "BbcOut node not found." << std::endl;
        //return DISCARDEVENT;
    }
    //------------------------------------------------------------------

    //-------------------TrigLvl1----------------------------------------
    d_trigl1 = NULL;
  	d_trigl1 = findNode::getClass<TrigLvl1>(topNode,"TrigLvl1");
    if (d_trigl1 == NULL)
    {
        cerr << "TrigLvl1 node not found." << std::endl;
        return DISCARDEVENT;
    }
    //------------------------------------------------------------------

    return 0;
}

//==============================================================

int MisalignSearchTree::End(PHCompositeNode *topNode)
{

  outfile->Write();
  outfile->Close();
  return 0;
}

void MisalignSearchTree::reset_variables()
{
	event    = 0;
	trigword = 0;
	bbcq     = -9999;
	for(int i=0; i<30; ++i)
	{
		for(int j=0; j<16; ++j)
		{
			hits[i][j] = 0;
		}
	}
	return;
}
