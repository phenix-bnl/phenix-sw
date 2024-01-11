//  This is implementation for prdf by Vlad Pantuev of S.Butsyk's 
//  efficiency calibration made originally for dst.
//           03-11-02
//
//  TKH--Insert hits for back side from stereo wires.
//  TKH--Change interface for slewing corrections.
//               11-25-2001
//

#include "mNewDchEfficiencyCalibrator.hh"
#include "PHDchHistogrammer.hh"
#include "dDchHitWrapper.h"
#include "DchTrack.h"

#include <PHIODataNode.h>
#include <PHTable.hh>
#include <PHObject.h>

#include <Event.h>

#include <TFile.h>
#include <TNtuple.h>

#include <iostream>
#include <cstring>
#include <sstream>

using namespace std;

typedef PHIODataNode<PHObject> PHObjectNode_t;
typedef PHIODataNode<PHTable> PHTableNode_t;

mNewDchEfficiencyCalibrator::mNewDchEfficiencyCalibrator() 
{
  memset(eff,0,sizeof(eff));
  memset(tot,0,sizeof(tot));
  EventRunNumber =-1;
  
}

PHBoolean mNewDchEfficiencyCalibrator::event(PHCompositeNode *root)
{
  
  topNode = root;
  PHObjectNode_t* phob;
  PHTableNode_t* phobT;
  
  PHPointerList<PHNode> nodes;
  PHNodeIterator i(root);
  nodes.clear();
  
  phob = static_cast < PHObjectNode_t * >(i.findFirst ("PHIODataNode", "DchTrack"));
  trackTable = dynamic_cast < DchTrack * >(phob->getData ());

  if (!trackTable) {
    cout <<  PHWHERE << "mNewDchEfficiencyCalibrator:: DchTrackTable not found "<< endl;
    return False;
  }
 
  phobT = static_cast < PHTableNode_t * >(i.findFirst ("PHIODataNode", "dDchHit"));
  hitLineTable = dynamic_cast < dDchHitWrapper * >(phobT->getData ());
  if (!hitLineTable) {
    cout << PHWHERE << "mNewDchEfficiencyCalibrator:: DchHitLineTable not found "<< endl;
    return False;
  } 
  
  Event              *eventPRDF;
  PHDataNode<Event>* eventNodePRDF;
 
  eventPRDF = 0;
  eventNodePRDF = (PHDataNode<Event>*)i.findFirst("PHDataNode", "PRDF");
  if (eventNodePRDF) {
  eventPRDF = eventNodePRDF->getData();
  if (eventPRDF) {
   EventRunNumber = eventPRDF->getRunNumber();     
  }else{
   cout <<  PHWHERE << "mNewDchEfficiencyCalibrator:: Event PRDF not found in node tree "<< endl;
  }    
  }else {
   cout <<  PHWHERE << "mNewDchEfficiencyCalibrator:: eventPRDFNode  not found "<< endl;
  }

  calculate();
  return True;
}

int mNewDchEfficiencyCalibrator::calculate()
{
      for (int yy=0 ; yy<2; yy++) {
	for (int zz=0 ; zz<2; zz++) {
	  for (int pp=0 ; pp<2; pp++) {
	    for (int cc=0 ; cc<80; cc++) {
	      firings[yy][zz][pp][cc]  = 0;
	      tr_id[yy][zz][pp][cc]  = -10;
	      first_wire[yy][zz][pp][cc]  = -10;
	      last_wire[yy][zz][pp][cc]  = -10;
	    }
	  }
	}
      } 
      
  int totalEntries = trackTable->get_DchNTrack();
  int ntr= totalEntries;     

      for (int i = 0; i< ntr; i++) {
	for (Int_t n=0 ; n<39; n++) {
	  Int_t hid =  trackTable->get_hits(i,n);	   
	  if ( hid!=-1 ) {
	    planeOfHit = hitLineTable->get_plane(hid);
	    
	    if (planeOfHit<12||(planeOfHit>19&&planeOfHit<32)) {
	      float dist_cut = 0.5;
	      
	      if (fabs(hitLineTable->get_distance(hid)-1.2)<dist_cut) { 
		
		dcarm  = trackTable->get_arm(i);
		dcside = trackTable->get_side(i);
		dccell = hitLineTable->get_cell(hid);
		
		if (planeOfHit<12) x_flag=0; else x_flag=1;
		
		if (firings[dcside][dcarm][x_flag][dccell] != -1) 
		  firings[dcside][dcarm][x_flag][dccell]++;
		if ((tr_id  [dcside][dcarm][x_flag][dccell] != i) && (tr_id  [dcside][dcarm][x_flag][dccell] != -10))
		  { 
		    firings[dcside][dcarm][x_flag][dccell] = -1;
		    first_wire[dcside][dcarm][x_flag][dccell] = -1; 
		  } else {
		    if (tr_id  [dcside][dcarm][x_flag][dccell] == -10) 
		      first_wire[dcside][dcarm][x_flag][dccell] = planeOfHit;
		  } 
		tr_id  [dcside][dcarm][x_flag][dccell] = i;
		s_sign [dcside][dcarm][x_flag][dccell] = planeOfHit%2;
		last_wire [dcside][dcarm][x_flag][dccell] = planeOfHit;
	      }
	    } 
	  }	  
	}
      }
         
      for (int yy=0 ; yy<2; yy++) {
	for (int zz=0 ; zz<2; zz++) {
	  for (int pp=0 ; pp<2; pp++) {
	    for (int cc=0 ; cc<80; cc++) {
	      int track_id = tr_id[yy][zz][pp][cc];
	      if (firings[yy][zz][pp][cc] > 3 && firings[yy][zz][pp][cc] < 7 && 
		  (last_wire[yy][zz][pp][cc]-first_wire[yy][zz][pp][cc])%2 == 0 &&
		  fabs(trackTable->get_alpha(track_id))<0.2) {
		
		int sign1 = s_sign[yy][zz][pp][cc];
		int plane_shift;
		if (pp==0) plane_shift = 0; else plane_shift = 20;
		
		for (int p_in1 = plane_shift+sign1; p_in1<plane_shift+12+sign1; p_in1+=2) tot[yy][zz][p_in1][cc]++;
		
		for (Int_t n=0 ; n<39; n++) {
		  Int_t hid = trackTable->get_hits(track_id,n);	  
		  
		  if ( hid!=-1 && cc==hitLineTable->get_cell(hid)) {
		    planeOfHit = hitLineTable->get_plane(hid);
		    		    
		    if (planeOfHit>=plane_shift&&planeOfHit<12+plane_shift&&(planeOfHit%2==sign1)) {
		      eff[yy][zz][planeOfHit][cc]++;
		    
		    }		    
		  }
		}
	      }
	    } 
	  }
	}	
    }  
  return 0;
}
int mNewDchEfficiencyCalibrator::saveToFile()
{
  total=-1;
  index=0;
  const char *string1="DchEfficiency";
  const char *string2  = ".Real";
  const char *string4  = ".root";

  Efficiency = new TNtuple("Efficiency","efficiency ntuple","side:arm:plane:cell:index:eff:statx:effx");// statx - statistics for eff

  TFile *DataFile;

  ostringstream string;
  string << string1 <<  EventRunNumber << string2;
  ostringstream string3;
  ostringstream string5;
  for (int i=0;i<1000;i++) 
    {
      string3.str("");
      string5.str("");
      string3 << string1 << i << EventRunNumber << string4;
      string5 << string1 << i << EventRunNumber << string2;
      
      DataFile = new TFile(string3.str().c_str(),"NEW",string1);
      if (DataFile->IsOpen()) goto Exit;
      delete DataFile;
    }
 Exit:   

  FILE* a = fopen(string5.str().c_str(),"w");

  for (int sss=0 ; sss<2; sss++) { // side
    for (int aaa=0 ; aaa<2; aaa++) { // arm 
      for (int pp=0 ; pp<40; pp++) { // plane
	for (int cc=0 ; cc<80; cc++) {  // cell
	  if (tot[sss][aaa][pp][cc]<6) tot_eff= 0; // here 6 is number of tracks in the cell/run. VP.
	  else tot_eff = (float)eff[sss][aaa][pp][cc]/(float)tot[sss][aaa][pp][cc];  
	  total++;
	  index=6400*sss+3200*aaa+80*pp+cc;

	  fprintf(a, "%d total %d index %f efficiency \n", total, index, tot_eff);

  // If you want to make Ntuple uncomment the following line	  
	  Efficiency->Fill(sss,aaa,pp,cc,index,tot_eff,tot[sss][aaa][pp][cc],eff[sss][aaa][pp][cc]);
	}
      }
    }
  }  

  fclose(a);

  // If you want to make Ntuple uncomment the following 8 lines
  if (DataFile) 
    {
      Efficiency->Write();
      DataFile->Close();
    }
  return 0;
}








