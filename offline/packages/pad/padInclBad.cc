// Class: padInclBad (implementation)
// 
// Created by: David Silvermyr
// 
// Description: 
// This class will take bad channels and bad ROCs from
// a PadCalibrationObject and modify the dPcXRaw tables

#include "PHNode.h"
#include "PHIODataNode.h"
#include "PHNodeIterator.h"
#include "PHTable.hh"
#include "PadAddressObject.hh"
#include "PadCalibrationObject.hh"
#include "padInclBad.hh"
#include "dPadRawWrapper.h"

#include <iostream>
#include <sstream>
#include <string>

using namespace std;

typedef PHIODataNode<PHTable> TableNode_t;
typedef PHIODataNode<TObject> ObjectNode_t;

//static int ievt = 0; // tmp debug

// Constructor for padInclBad
padInclBad::padInclBad()
{
  //  ievt = 0; // tmp debug
  PadCalObj = new  PadCalibrationObject;
  addressObj = new  PadAddressObject;

  Debug = 0; // do not Debug
  // From 6 June 2001, this is obsolete and just waiting to be removed

  inclBadROCs=1; // include bad ROCs in the table modifications
  inclBadChs=1; // include bad channels in the table modifications

  NumInclBadROCs=0;
  NumInclBadChs=0;

  // default is to just remove all hot channels/ROCs and ROCs with the wrong internal event number
  // a.k.a unSynch
  rmHotROCs=1;
  rmInactROCs=0;
  addHotROCs=0;
  addInactROCs=0;

  rmUnSynchROCs=1;
  addUnSynchROCs=0;

  rmHotChs=1;
  rmInactChs=0;
  addHotChs=0;
  addInactChs=0;

  // explcitly clear/init sets also
  addset.clear();
  removeset.clear();
}  // end method padInclBad::padInclBad

// Destructor for padInclBad
padInclBad::~padInclBad() {
  delete PadCalObj;
  delete addressObj;
}  // end method padInclBad::~padInclBad

// Event method for padInclBad
PHBoolean padInclBad::event(PHCompositeNode* topNode) {

  //  ievt++; // tmp debug
  // let's first see what PadCalObj contains, before starting the modifying..
  // Check that it was init./setup correctly

  if ( (inclBadROCs==1) || (inclBadChs==1) ) {
    if (PadCalObj->getiFlag()!=0) {
      cerr << "padInclBad::event ERROR PadCalObj has not been initialized! Use FetchCalDataFromFile or FetchCalDataFromObjy first and try again\n";
      return False;
    } 
  }
  else {
    return True; // couldn't fail with that one could we..
  }

  PHNodeIterator topIter(topNode);

  /* CFM November 29, 2001
     Compiler was complaining about the dPcxRaw being possibly uninitialized
     While that could happen, there were already error messages to indicate a problem.
     Right way is to throw an exception, etc., but we will leave it simple for now.
     Setting the pointers initially to 0 should keep the compiler happy
  */

  TableNode_t *dPc1RawNode;
  dPadRawWrapper *dPc1Raw = 0;
  TableNode_t *dPc2RawNode;
  dPadRawWrapper *dPc2Raw = 0;
  TableNode_t *dPc3RawNode;
  dPadRawWrapper *dPc3Raw = 0;

  // Find the input tables
  dPc1RawNode = static_cast<TableNode_t*>(topIter.findFirst("PHIODataNode","dPc1Raw"));
  if (!dPc1RawNode) {
    cout << "padInclBad::event ERROR: dPc1Raw not found.\n";
    return False;
  }
  else dPc1Raw = static_cast<dPadRawWrapper*>(dPc1RawNode->getData());
  
  dPc2RawNode = static_cast<TableNode_t*>(topIter.findFirst("PHIODataNode","dPc2Raw"));
  if (!dPc2RawNode) {
    cout << "padInclBad::event ERROR: dPc2Raw not found.\n";
    return False;
  }
  else dPc2Raw = static_cast<dPadRawWrapper*>(dPc2RawNode->getData());

  dPc3RawNode = static_cast<TableNode_t*>(topIter.findFirst("PHIODataNode","dPc3Raw"));
  if (!dPc3RawNode) {
    cout << "padInclBad::event ERROR: dPc3Raw not found.\n";
    return False;
  }
  else dPc3Raw = static_cast<dPadRawWrapper*>(dPc3RawNode->getData());

  /*
  PHBoolean statuspre = printToFile(dPc1Raw, 0, "pre", ievt); // tmp debug
  statuspre = printToFile(dPc2Raw, 1, "pre", ievt); // tmp debug
  statuspre = printToFile(dPc3Raw, 2, "pre", ievt); // tmp debug
  */
  Pad pad;
  ioset = addset; // we initialize with the bad stuff we want to add

  // Now, we'll go through the input tables and add everything that
  // is not in the removeset
  unsigned int i;
  // PC1
  pad.pc = 0;
  for (i=0; i<dPc1Raw->RowCount(); i++) {
    pad.arm = dPc1Raw->get_arm(i);
    pad.side = dPc1Raw->get_side(i);
    pad.sector = dPc1Raw->get_sector(i);
    pad.padz = dPc1Raw->get_padz(i);
    pad.padx = dPc1Raw->get_padx(i);
    pad.padtype = dPc1Raw->get_padtype(i); // PN 0; // padtype=0 <=> ok, channel
    pad.id = dPc1Raw->get_id(i); // PN

    iter = removeset.find(pad);
    if ( iter == removeset.end() )
      { // not to be removed, insert this one
	ioset.insert(pad);    
      }
  }
  // PC2
  pad.pc = 1;
  for (i=0; i<dPc2Raw->RowCount(); i++) {
    pad.arm = dPc2Raw->get_arm(i);
    pad.side = dPc2Raw->get_side(i);
    pad.sector = dPc2Raw->get_sector(i);
    pad.padz = dPc2Raw->get_padz(i);
    pad.padx = dPc2Raw->get_padx(i);
    pad.padtype = dPc2Raw->get_padtype(i); // PN 0; // padtype=0 <=> ok, channel
    pad.id = dPc2Raw->get_id(i); // PN

    iter = removeset.find(pad);
    if ( iter == removeset.end() )
      { // not to be removed, insert this one
	ioset.insert(pad);    
      }
  }
  // PC3
  pad.pc = 2;
  for (i=0; i<dPc3Raw->RowCount(); i++) {
    pad.arm = dPc3Raw->get_arm(i);
    pad.side = dPc3Raw->get_side(i);
    pad.sector = dPc3Raw->get_sector(i);
    pad.padz = dPc3Raw->get_padz(i);
    pad.padx = dPc3Raw->get_padx(i);
    pad.padtype = dPc3Raw->get_padtype(i); // PN 0; // padtype=0 <=> ok, channel
    pad.id = dPc3Raw->get_id(i); // PN

    iter = removeset.find(pad);
    if ( iter == removeset.end() )
      { // not to be removed, insert this one
	ioset.insert(pad);    
      }
  }

  // let's fill out the dPcXRaw tables with the updated information
  int npads[3] = { 0, 0, 0};
  for(iter = ioset.begin(); iter != ioset.end(); iter++)
    {
      pad = (*iter);

      //  PC1
      if (pad.pc == 0) {
	i = npads[0]; // for readability
	dPc1Raw->set_arm(i, pad.arm);
	dPc1Raw->set_side(i, pad.side);
	dPc1Raw->set_sector(i, pad.sector);
	dPc1Raw->set_padz(i, pad.padz);
	dPc1Raw->set_padx(i, pad.padx);
	dPc1Raw->set_padtype(i, pad.padtype);
	dPc1Raw->set_id(i, pad.id);
	npads[0]++;
      }

      //  PC2
      if (pad.pc == 1) {
	i = npads[1]; // for readability
	dPc2Raw->set_arm(i, pad.arm);
	dPc2Raw->set_side(i, pad.side);
	dPc2Raw->set_sector(i, pad.sector);
	dPc2Raw->set_padz(i, pad.padz);
	dPc2Raw->set_padx(i, pad.padx);
	dPc2Raw->set_padtype(i, pad.padtype);
	dPc2Raw->set_id(i, pad.id);
	npads[1]++;
      }

      //  PC3
      if (pad.pc == 2) {
	i = npads[2]; // for readability
	dPc3Raw->set_arm(i, pad.arm);
	dPc3Raw->set_side(i, pad.side);
	dPc3Raw->set_sector(i, pad.sector);
	dPc3Raw->set_padz(i, pad.padz);
	dPc3Raw->set_padx(i, pad.padx);
	dPc3Raw->set_padtype(i, pad.padtype);
	dPc3Raw->set_id(i, pad.id);
	npads[2]++;
      }

    } // iter

  // set the counts
  dPc1Raw->SetRowCount(npads[0]);
  dPc2Raw->SetRowCount(npads[1]);
  dPc3Raw->SetRowCount(npads[2]);
  /*
  PHBoolean statuspost = printToFile(0, "post", ievt); // tmp debug
  statuspost = printToFile(1, "post", ievt); // tmp debug
  statuspost = printToFile(2, "post", ievt); // tmp debug
  */
  return True;
}  // end method padInclBad::event

PHBoolean 
padInclBad::FetchCalDataFromFiles()
{ // from default ASCII files

  PHBoolean status = PadCalObj->FetchFromFile();
  if(!status) {
    cerr << "padInclBad::FetchCalDataFromFiles() ERROR fetching data from default files" << endl;
    return False;
  }
  status = FillBadSets();
  return status;
} // end method padInclBad::FetchCalDataFromFiles


PHBoolean 
padInclBad::FetchCalDataFromFiles(const char* filebadch, const char* filebadroc) 
{ // from specific ASCII files

  PHBoolean status = PadCalObj->FetchFromFile(filebadch,filebadroc);
  if(!status) {
    cerr << "padInclBad::FetchCalDataFromFiles() ERROR fetching data from specified files" << endl;
    return False;
  }
  status = FillBadSets();
  return status;
} // end method padInclBad::FetchCalDataFromFiles

PHBoolean 
padInclBad::FetchCalDataFromObjy(PHTimeStamp &TS)
{ // from Objectivity

  PadCalObj->setTimeStamp(TS);
  PHBoolean status = PadCalObj->FetchBadROCObjy();
  if(!status) {
    cerr << "padInclBad::FetchCalDataFromObjy() ERROR fetching data from Objectivity" << endl;
    return False;
  }
  status = PadCalObj->FetchBadChObjy(); // also bad channel info
  if(!status) {
    cerr << "padInclBad::FetchCalDataFromObjy() ERROR fetching data from Objectivity" << endl;
    return False;
  }
  status = FillBadSets();
  return status;
} // end method padInclBad::FetchCalDataFromObjy

PHBoolean
padInclBad::FillBadSets()
{
  // decode info from PadCalObj; and store the bad channels
  // in the 'badset', following what's been setup

  // first clear our sets
  removeset.clear();
  addset.clear();

  PHBoolean status = True;
  if (inclBadROCs == 1) {   // incl bad ROCs ?
   status = FillBadSetsWithROCInfo();
  }

  if (inclBadChs == 1) {   // incl bad ROCs ?
    status = FillBadSetsWithChInfo();
  }
  //  status = printBadToFile(); // tmp debug
  return status;
}

PHBoolean
padInclBad::FillBadSetsWithROCInfo()
{
  int minpadz,maxpadz,minpadx,maxpadx;
  int padz, padx;
  Pad pad;

  NumInclBadROCs = PadCalObj->getNumberBadROC();
  
  // ***** REMOVE PART *****
  if (rmHotROCs==1 || rmInactROCs==1 || rmUnSynchROCs==1) {
    // Let's start the remove ROCs part..
    for (int i=0; i<PadCalObj->getNumberBadROC(); i++) {
      
      pad.pc = PadCalObj->getDetBadROC(i);
      pad.arm = PadCalObj->getArmBadROC(i);
      pad.side = PadCalObj->getSideBadROC(i);
      pad.sector = PadCalObj->getSectorBadROC(i);
      maxpadz = PadCalObj->getMaxPadzBadROC(i);
      maxpadx = PadCalObj->getMaxPadxBadROC(i);
      minpadz = PadCalObj->getMinPadzBadROC(i);
      minpadx = PadCalObj->getMinPadxBadROC(i);
      pad.id =-1;
      
      for (padz=minpadz; padz<=maxpadz;padz++) {
	for (padx=minpadx; padx<=maxpadx;padx++) {
	  
	  pad.padtype = ROCtypeToPadType(PadCalObj->getROCtypeBadROC(i),
					 pad.pc, padz, padx);
	  
	  // check first if we should try to remove it or not..
	  if ( ( (pad.padtype==-1) && (rmUnSynchROCs==1) ) || 
	       ( (pad.padtype==1) && (rmInactROCs==1) ) ||
	       ( (pad.padtype==2) && (rmHotROCs==1) ) ) { // ok
	    
	    pad.padz = padz;
	    pad.padx = padx;
	    
	    // let's add it to the removeset
	    removeset.insert(pad);
	  }
	} // padx
      } //padz
    } // numberbadroc
  } // do rm
  
  // ***** ADDITION PART *****
  // add borders of all ROCs (put them last in their respective tables)
  if (addHotROCs==1 || addInactROCs==1 || addUnSynchROCs==1) {
    
    for (int i=0; i<PadCalObj->getNumberBadROC(); i++) {
      
      pad.pc = PadCalObj->getDetBadROC(i);
      pad.arm = PadCalObj->getArmBadROC(i);
      pad.side = PadCalObj->getSideBadROC(i);
      pad.sector = PadCalObj->getSectorBadROC(i);
      maxpadz = PadCalObj->getMaxPadzBadROC(i);
      maxpadx = PadCalObj->getMaxPadxBadROC(i);
      minpadz = PadCalObj->getMinPadzBadROC(i);
      minpadx = PadCalObj->getMinPadxBadROC(i);
      pad.id =-1;
      
      for (padz=minpadz; padz<=maxpadz;padz++) {
	for (padx=minpadx; padx<=maxpadx;padx++) {
	  
	  if ( ( (padz==minpadz) || (padz==maxpadz) ) || 
	       ( (padx==minpadx) || (padx==maxpadx) ) ) {
	    // we should only add ROC border entries
	    
	    pad.padtype = ROCtypeToPadType(PadCalObj->getROCtypeBadROC(i),
					   pad.pc, padz, padx);
	    // check first if we should try to add it or not..
	    if ( ( (pad.padtype==-1) && (addUnSynchROCs==1) ) || 
		 ( (pad.padtype==1) && (addInactROCs==1) ) ||
		 ( (pad.padtype==2) && (addHotROCs==1) ) ) { // ok
	      
	      pad.padz = padz;
	      pad.padx = padx;
	      
	      // let's add it to the addset
	      addset.insert(pad);
	    }	    
	  }
	} //padx
      } //padz
    } // numberbadroc
  } //add

  return True;
} 

int 
padInclBad::ROCtypeToPadType(short badroctype,
			     short det,
			     short padz,
			     short padx)
{
  short padtype=0;
  short badtgl[3],channelid,tglvalue;

  if (badroctype==-1) padtype=-1;
  else if ( (badroctype >= 0) && (badroctype<= 222) ) { // allowed index range 
    badtgl[0]=badroctype/100;
    badtgl[1]=(badroctype%100)/10;
    badtgl[2]=badroctype%10;
    // so, which tgl do we have? use PadAddressObject..
    channelid = addressObj->getChannelid(det,padz,padx);
    tglvalue = addressObj->getTgl(channelid); // 1 to 3
    tglvalue--; // 0 to 2..
    if ( (tglvalue>=0) && (tglvalue<=2) ) 
      padtype=badtgl[tglvalue];
    else padtype=0; 

  }
  return padtype;
}

PHBoolean 
padInclBad::FillBadSetsWithChInfo()
{
  Pad pad;

  NumInclBadChs = PadCalObj->getNumberBadCh();

  // ***** REMOVE PART *****
  if (rmHotChs==1 || rmInactChs==1) {
    // Let's start the remove Chs part..
    for (int i=0; i<PadCalObj->getNumberBadCh(); i++) {
      
      pad.pc = PadCalObj->getDetBadCh(i);
      pad.arm = PadCalObj->getArmBadCh(i);
      pad.side = PadCalObj->getSideBadCh(i);
      pad.sector = PadCalObj->getSectorBadCh(i);
      pad.padz = PadCalObj->getPadzBadCh(i);
      pad.padx = PadCalObj->getPadxBadCh(i);
      pad.padtype = PadCalObj->getPadtypeBadCh(i);
      pad.id = -1;

      // check first if we should try to remove it or not..
      if ( ( (pad.padtype==1) && (rmInactChs==1) ) ||
	   ( (pad.padtype==2) && (rmHotChs==1) ) ) { // ok
	
	// let's add it to the removeset
	removeset.insert(pad);
      }
    } // ch loop
  } // rm check

  // ***** ADDITION PART *****
  if (addHotChs==1 || addInactChs==1) {
    // Let's start the add Chs part..
    for (int i=0; i<PadCalObj->getNumberBadCh(); i++) {

      pad.pc = PadCalObj->getDetBadCh(i);
      pad.arm = PadCalObj->getArmBadCh(i);
      pad.side = PadCalObj->getSideBadCh(i);
      pad.sector = PadCalObj->getSectorBadCh(i);
      pad.padz = PadCalObj->getPadzBadCh(i);
      pad.padx = PadCalObj->getPadxBadCh(i);
      pad.padtype = PadCalObj->getPadtypeBadCh(i);
      pad.id = -1;
      
      // check first if we should try to add it or not..
      if ( ( (pad.padtype==1) && (addInactChs==1) ) ||
	   ( (pad.padtype==2) && (addHotChs==1) ) ) { // ok
	
	// let's add it to the addset
	addset.insert(pad);
      }
    } // ch loop
  } // add check
  return True;
}

void 
padInclBad::print() 
{
  cout << "padInclBad::print \n";

  // setup info
  cout << "Debug = " << Debug << "\n";
  cout << "inclBadROCs  = " << inclBadROCs  << "\n";
  cout << "inclBadChs   = " << inclBadChs  << "\n";
  cout << endl;

  // calibration object info
  if (PadCalObj->getiFlag()!=0) {
    cout << "PadCalObj has not been initialized\n";
  } 
  else {
    cout << "PadCalObj: Number of bad ROCs= " << PadCalObj->getNumberBadROC() << endl;
    cout << "PadCalObj: Number of bad channels= " << PadCalObj->getNumberBadCh() << endl;
  }
  cout << endl;

  // table info
  cout << "NumInclBadROCs = " << NumInclBadROCs << "\n";
  cout << "NumInclBadChs = " << NumInclBadChs << "\n";
  cout << endl;

}  // end method padInclBad::print

PHBoolean 
padInclBad::printToFile(const int pc, const char * info, 
			const int ievt) 
{
  // Open the file
  ostringstream filename;

  if ( (pc<0) || (pc>2) ) {
    cerr << "padInclBad::printToFile pc is out of bounds.\n";
    return False;
  }  
  filename << "data_pc" << pc << "_" << info << "_" << ievt << ".txt";

  FILE *fout;
  if((fout = fopen(filename.str().c_str(),"w"))==NULL){ 
    cerr << "padInclBad::printToFile Cannot open file.\n";
    return False;
  }
  /*
  printf("Printing raw data table %d (PC%d) to file %s\n",pc,pc+1,filename);
  // first line tells us how many entries that are in the table
  fprintf(fout,"%d entries. Format is\n", ioset.size()); 
  fprintf(fout,"pc\t arm\t side\t sector\t padz\t padx\t padtype\t id\n");
  */
  Pad pad;
  for(iter = ioset.begin(); iter != ioset.end(); iter++)
    {
      pad = (*iter);
      if (pad.pc == pc) {
	fprintf(fout,"%d\t %d\t %d\t %d\t %d\t %d\t %d\t %d\n",
		pad.pc, pad.arm, pad.side, pad.sector, pad.padz, pad.padx,
		pad.padtype, pad.id);
      }
    }
  
  fclose(fout);  
  return True;
}  // end method padInclBad::printToFile

PHBoolean 
padInclBad::printToFile(const dPadRawWrapper *dPcRaw,
			const int pc, const char * info, 
			const int ievt) 
{
  // Open the file
  char filename[80];
  sprintf(filename,"data_pc%d_%s_%d.txt", pc, info, ievt);

  if ( (pc<0) || (pc>2) ) {
    cerr << "padInclBad::printToFile pc is out of bounds.\n";
    return False;
  }  
  FILE *fout;
  if((fout = fopen(filename,"w"))==NULL){ 
    cerr << "padInclBad::printToFile Cannot open file.\n";
    return False;
  }

  /*
  printf("Printing raw data table %d (PC%d) to file %s\n",pc,pc+1,filename);
  // first line tells us how many entries that are in the table
  fprintf(fout,"%d entries. Format is\n", ioset.size()); 
  fprintf(fout,"pc\t arm\t side\t sector\t padz\t padx\t padtype\t id\n");
  */
  for (unsigned int i=0; i<dPcRaw->RowCount(); i++) {
      fprintf(fout,"%d\t %d\t %d\t %d\t %d\t %d\t %d\t %d\n",
	      pc, dPcRaw->get_arm(i),        
	      dPcRaw->get_side(i),
	      dPcRaw->get_sector(i),
	      dPcRaw->get_padz(i),      
	      dPcRaw->get_padx(i),      
	      dPcRaw->get_padtype(i),
	      dPcRaw->get_id(i));
  }
  fclose(fout);  
  return True;
}  // end method padInclBad::printToFile

PHBoolean 
padInclBad::printBadToFile()
{
  // Open the file
  string filename;

  filename = "add.txt";
  FILE *fout;
  if((fout = fopen(filename.c_str(),"w"))==NULL){ 
    cerr << "padInclBad::printBadToFile Cannot open file.\n";
    return False;
  }
  /*
  printf("Printing add data table to file %s\n",filename);
  // first line tells us how many entries that are in the table
  fprintf(fout,"%d entries. Format is\n", addset.size()); 
  fprintf(fout,"pc\t arm\t side\t sector\t padz\t padx\t padtype\t id\n");
  */
  Pad pad;
  for(iter = addset.begin(); iter != addset.end(); iter++)
    {
      pad = (*iter);
      fprintf(fout,"%d\t %d\t %d\t %d\t %d\t %d\t %d\t %d\n",
	      pad.pc, pad.arm, pad.side, pad.sector, pad.padz, pad.padx,
	      pad.padtype, pad.id);
    }
  fclose(fout);  


  filename = "remove.txt";

  if((fout = fopen(filename.c_str(),"w"))==NULL){ 
    cerr << "padInclBad::printBadToFile Cannot open file.\n";
    return False;
  }
  /*
  printf("Printing remove data table to file %s\n",filename);
  // first line tells us how many entries that are in the table
  fprintf(fout,"%d entries. Format is\n", removeset.size()); 
  fprintf(fout,"pc\t arm\t side\t sector\t padz\t padx\t padtype\t id\n");
  */
  for(iter = removeset.begin(); iter != removeset.end(); iter++)
    {
      pad = (*iter);
      fprintf(fout,"%d\t %d\t %d\t %d\t %d\t %d\t %d\t %d\n",
	      pad.pc, pad.arm, pad.side, pad.sector, pad.padz, pad.padx,
	      pad.padtype, pad.id);
    }
  
  fclose(fout);  

  return True;
}  
