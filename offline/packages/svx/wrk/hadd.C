//macro to add histogram files
//NOTE: This macro is kept for back compatibility only.
//Use instead the executable $ROOTSYS/bin/hadd
//
//This macro will add histograms from a list of root files and write them
//to a target root file. The target file is newly created and must not be
//identical to one of the source files.
//
//Author: Sven A. Schmidt, sven.schmidt@cern.ch
//Date:   13.2.2001

//This code is based on the hadd.C example by Rene Brun and Dirk Geppert,
//which had a problem with directories more than one level deep.
//(see macro hadd_old.C for this previous implementation).
//
//The macro from Sven has been enhanced by
//   Anne-Sylvie Nicollerat <Anne-Sylvie.Nicollerat@cern.ch>
// to automatically add Trees (via a chain of trees).
//
//To use this macro, modify the file names in function hadd.
//
//NB: This macro is provided as a tutorial.
//    Use $ROOTSYS/bin/hadd to merge many histogram files



#include <string.h>
#include "TChain.h"
#include "TFile.h"
#include "TH1.h"
#include "TH2.h"
#include "TTree.h"
#include "TKey.h"
#include "Riostream.h"

TList *FileList;
TFile *Target;

static const Int_t nmax = 6000;
void MergeRootfile( TDirectory *target, TList *sourcelist );


void hadd() {
  // in an interactive ROOT session, edit the file names
  // Target and FileList, then
  // root > .L hadd.C
  // root > hadd()

  Target = TFile::Open( "raterun_all.root", "RECREATE" );

  FileList = new TList();
  FileList->Add( TFile::Open("raterun_346973_347299.root") );
  FileList->Add( TFile::Open("raterun_347300_347599.root") );
  FileList->Add( TFile::Open("raterun_347600_347899.root") );
  FileList->Add( TFile::Open("raterun_347900_348199.root") );
  FileList->Add( TFile::Open("raterun_348200_348499.root") );
  FileList->Add( TFile::Open("raterun_348500_348799.root") );
  FileList->Add( TFile::Open("raterun_348800_349099.root") );
  FileList->Add( TFile::Open("raterun_349100_349399.root") );
  FileList->Add( TFile::Open("raterun_349400_349699.root") );
  MergeRootfile( Target, FileList );

}

void MergeRootfile( TDirectory *target, TList *sourcelist ) {

  //  cout << "Target path: " << target->GetPath() << endl;
  TString path( (char*)strstr( target->GetPath(), ":" ) );
  path.Remove( 0, 2 );

  TFile *first_source = (TFile*)sourcelist->First();
  first_source->cd( path );
  TDirectory *current_sourcedir = gDirectory;
  //gain time, do not add the objects in the list in memory
  Bool_t status = TH1::AddDirectoryStatus();
  TH1::AddDirectory(kFALSE);

  // loop over all keys in this directory
  TChain *globChain = 0;
  TIter nextkey( current_sourcedir->GetListOfKeys() );
  TKey *key, *oldkey=0;
  while ( (key = (TKey*)nextkey())) {

    //keep only the highest cycle number for each key
    if (oldkey && !strcmp(oldkey->GetName(),key->GetName())) continue;

    // read object from first source file
    first_source->cd( path );
    TObject *obj = key->ReadObj();
    cout << "key->GetName() = " << key->GetName() << endl;

    if ( obj->IsA()->InheritsFrom( TGraph::Class() ) ) {

      Double_t xm[nmax]={0};
      Double_t ym[nmax]={0};

      TGraph *hhh1 = (TGraph*)obj;
      cout << "TGraph found!!!" << hhh1->GetName() << endl;
      const Int_t n1 = hhh1->GetN();
      Int_t ndata=0;
      Double_t *x1 = hhh1->GetX();
      Double_t *y1 = hhh1->GetY();
      for (Int_t ix=0;ix<n1;ix++) {
	xm[ndata+ix] = x1[ix];
	ym[ndata+ix] = y1[ix];
      }
      ndata += n1;


      // loop over all source files and add the content of the
      // correspondant histogram to the one pointed to by "h2"
      TFile *nextsource = (TFile*)sourcelist->After( first_source );
      while ( nextsource ) {

	// make sure we are at the correct directory level by cd'ing to path
	nextsource->cd( path );
	//	TKey *key2 = (TKey*)gDirectory->GetListOfKeys()->FindObject(hhh1->GetName());
	TKey *key2 = (TKey*)gDirectory->GetListOfKeys()->FindObject(key->GetName());
	if (key2) {
	  TGraph *hhh2 = (TGraph*)key2->ReadObj();
	  cout << "Same name TGraph found!!!" << key2->GetName() << endl;

	  const Int_t n2 = hhh2->GetN();
	  Double_t *x2 = hhh2->GetX();
	  Double_t *y2 = hhh2->GetY();

	  if (ndata+n2 <= nmax) {
	    for (Int_t ix=0;ix<n2;ix++) {
	      xm[ndata+ix] = x2[ix];
	      ym[ndata+ix] = y2[ix];
	    }
	    ndata += n2;
	  } else {
	    cerr << "ndata+n2" << ndata+n2 << " larger than nmax " << nmax << endl;
	  }

	  //	  cout << "hh1 entries (after merge) = " << hhhh1->GetEntries() << endl;
	  delete hhh2;

	}// if (key2)

	nextsource = (TFile*)sourcelist->After( nextsource );
      }

      obj = new TGraph(ndata, xm, ym);

      cout << "end merging TGraph" << endl;
    } else if ( obj->IsA()->InheritsFrom( TH2::Class() )) {

      // descendant of TH2 -> merge it

      cout << "Merging TH2 histogram " << obj->GetName() << endl;
      TH2 *hh1 = (TH2*)obj;
      cout << "hh1 entries (before merge) = " << hh1->GetEntries() << endl;

      // loop over all source files and add the content of the
      // correspondant histogram to the one pointed to by "h2"
      TFile *nextsource = (TFile*)sourcelist->After( first_source );
      while ( nextsource ) {

	// make sure we are at the correct directory level by cd'ing to path
	nextsource->cd( path );
	TKey *key2 = (TKey*)gDirectory->GetListOfKeys()->FindObject(hh1->GetName());
	if (key2) {
	  TH2 *hh2 = (TH2*)key2->ReadObj();
	  hh1->Add( hh2 );
	  cout << "hh1 entries (after merge) = " << hh1->GetEntries() << endl;
	  delete hh2;
	}

	nextsource = (TFile*)sourcelist->After( nextsource );
      }
    }
    else if ( obj->IsA()->InheritsFrom( TH1::Class() ) ) {
      // descendant of TH1 -> merge it

      cout << "Merging TH1 histogram " << obj->GetName() << endl;
      TH1 *h1 = (TH1*)obj;
      cout << "h1 entries (before merge) = " << h1->GetEntries() << endl;
      // loop over all source files and add the content of the
      // correspondant histogram to the one pointed to by "h1"
      TFile *nextsource = (TFile*)sourcelist->After( first_source );
      while ( nextsource ) {

	// make sure we are at the correct directory level by cd'ing to path
	nextsource->cd( path );
	TKey *key2 = (TKey*)gDirectory->GetListOfKeys()->FindObject(h1->GetName());
	if (key2) {
	  TH1 *h2 = (TH1*)key2->ReadObj();
	  h1->Add( h2 );
	  cout << "h1 entries (after merge) = " << h1->GetEntries() << endl;
	  delete h2;
	}

	nextsource = (TFile*)sourcelist->After( nextsource );
      }
    }
    else if ( obj->IsA()->InheritsFrom( TTree::Class() ) ) {

      cout << "TTree found" << endl;
      // loop over all source files create a chain of Trees "globChain"
      const char* obj_name= obj->GetName();

      globChain = new TChain(obj_name);
      globChain->Add(first_source->GetName());
      TFile *nextsource = (TFile*)sourcelist->After( first_source );
      //      const char* file_name = nextsource->GetName();
      // cout << "file name  " << file_name << endl;
      while ( nextsource ) {

	globChain->Add(nextsource->GetName());
	nextsource = (TFile*)sourcelist->After( nextsource );
      }

    } else if ( obj->IsA()->InheritsFrom( TDirectory::Class() ) ) {
      // it's a subdirectory

      cout << "Found subdirectory " << obj->GetName() << endl;

      // create a new subdir of same name and title in the target file
      target->cd();
      TDirectory *newdir = target->mkdir( obj->GetName(), obj->GetTitle() );

      // newdir is now the starting point of another round of merging
      // newdir still knows its depth within the target file via
      // GetPath(), so we can still figure out where we are in the recursion
      MergeRootfile( newdir, sourcelist );

    } else {
      cout << "other object found" << endl;

      // object is of no type that we know or can handle
      //      cout << "Unknown object type, name: "
      //	   << obj->GetName() << " title: " << obj->GetTitle() << endl;
    }

    // now write the merged histogram (which is "in" obj) to the target file
    // note that this will just store obj in the current directory level,
    // which is not persistent until the complete directory itself is stored
    // by "target->Write()" below
    if ( obj ) {
      target->cd();

      //!!if the object is a tree, it is stored in globChain...
      if(obj->IsA()->InheritsFrom( TTree::Class() ))
	globChain->Merge(target->GetFile(),0,"keep");
      else
	obj->Write( key->GetName() );
    }

  } // while ( ( TKey *key = (TKey*)nextkey() ) )

  // save modifications to target file
  target->SaveSelf(kTRUE);
  TH1::AddDirectory(status);
}

