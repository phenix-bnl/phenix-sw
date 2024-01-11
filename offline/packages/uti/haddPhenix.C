/*

  This program will add histograms from a list of root files and write them
  to a target root file. The target file is newly created and must not be
  identical to one of the source files.

  Syntax:

       hadd targetfile source1 source2 ...

  if the source files contains histograms and Trees, one can skip 
  the Trees with
       hadd -T targetfile source1 source2 ...
  
  Authors: Rene Brun, Dirk Geppert, Sven A. Schmidt, sven.schmidt@cern.ch

 */

#include <iostream>
#include <string>
#include <list>
#include <functional>
#include <vector>
#include <popt.h>
#include <boost/scoped_ptr.hpp>

#include <TChain.h>
#include <TFile.h>
#include <TH1.h>
#include <TKey.h>
#include <Riostream.h>
#include <THmulf.h>

int noTrees = 0;
int help = 0;
int hmerge_style = 0;

// intermediate container for filenames.  to be filled by either popt or
// standard version of the commandline parsing, thus provides a standard
// source of filename strings for loading the TList of TFile*'s.
std::vector<std::string> cmdargs;

void Setup( std::string& target, TList *sourcelist );
void MergeRootfile( TDirectory *target, TList *sourcelist );

#if HAVE_POPT_H
struct poptOption optionsTable[] = {
  { "h", 'h', POPT_ARG_NONE, &help, 0, "produce help message", 0},
  { "skip-trees", 'T', POPT_ARG_NONE, &noTrees, 0, "skip over TTrees when merging", 0 },
  { "hmerge", '\0', POPT_ARG_NONE, &hmerge_style, 0, "Arg list is PHENIX hmerge style (haddPhenix listfile targetfile) (NOT YET IMPLEMENTED)", 0 },
  POPT_AUTOHELP
  { NULL, 0, 0, NULL, 0 }
};

// POPT version
static void
usage(poptContext optCon, int exitcode, const char *error, const char *addl) {
  poptPrintUsage(optCon, stderr, 0);
  if ( error ) std::cerr << error;
  if ( addl ) std::cerr << ": " << addl;
  std::cerr << std::endl;
  exit(exitcode);
}

static void
parse_args(int& argc, const char** argv)
{
  poptContext optCon = poptGetContext(NULL, argc, argv, optionsTable, 0);
  poptSetOtherOptionHelp(optCon, "target source1 source2 ... ");

  if (argc < 2) {
    poptPrintUsage(optCon, stderr, 0);
    exit(1);
  }

  // Now do options processing, get other commandline args?
  char c;
  while ((c = poptGetNextOpt(optCon)) >= 0) {  }

  if (c < -1) {
    // an error occurred during option processing
    std::cerr << poptBadOption(optCon, POPT_BADOPTION_NOALIAS) << ": "
	      << poptStrerror(c);
  }

  if ( help ) usage(optCon, 0, 0, 0);

  while ( const char *ptr = poptGetArg(optCon) )
    {
      cmdargs.push_back(ptr);
    }

  if ( cmdargs.size() < 2 ) usage(optCon, 1, "Not enough required args", 0);

  poptFreeContext(optCon);
  return;
}

#else 

// "Standard" version of usage
//
static void
usage(int val)
{
  std::cout << "Usage: haddPhenix targetfile source1 source2 [source3 ...]" << std::endl;
  std::cout << "This program will add histograms from a list of root files and write them" << std::endl;
  std::cout << "to a target root file. The target file is newly created and must not be" << std::endl;
  std::cout << "identical to one of the source files." << std::endl;
  std::cout << "Supply at least two source files for this to make sense... ;-)" << std::endl;
  std::cout << "If the first argument is -T, Trees are not merged" <<std::endl;
  exit(val);
}

#endif

int main( int argc, char **argv ) {

#if HAVE_POPT_H

  parse_args(argc,const_cast<const char **>(argv));

#else
  if ( argc < 3 || "-h" == std::string(argv[1]) || "--help" == std::string(argv[1]) )
    {
      usage(1);
    }

  int ffirst = 2;

  if ("-T" == std::string(argv[1])) {
     noTrees = kTRUE;
     ffirst = 3;
  }

  for ( int i = ffirst; i < argc; i++ ) {
    cmdargs.push_back(argv[i]);
  }

#endif

  std::cout << "Skipping TTrees in this merge: " << (noTrees ? "true" : "false") << std::endl;

  std::string tname;

  TList FileList;

  Setup(tname,&FileList);

  if ( FileList.GetSize() <= 0 ) 
    {
      std::cout << "No valid input files found, bailing out." << std::endl;
      return 1;
    }

  TFile* Target = TFile::Open( tname.c_str(), "RECREATE" );

  if ( ! Target ) return 2; // Again, ROOT will print an error message about this...
  
  MergeRootfile( Target, &FileList );

  return 0;
}

void
Setup( std::string& tname, TList* FileList )
{
  std::vector<std::string> filenames;

  if ( hmerge_style )
    {
      filenames.push_back(cmdargs[1]);

      std::ifstream f(cmdargs[0].c_str());
      char line[4092];
      while ( f.getline(line,sizeof(line)) )
	{
	  filenames.push_back(line);
	}
    }
  else
    {
      std::copy(cmdargs.begin(),cmdargs.end(),std::back_inserter(filenames));
    }

  //const char* tname = filenames[0];
  tname = filenames[0];
  std::cout << "Target file: " << tname << std::endl;
  
  for ( unsigned int i = 1; i < filenames.size(); i++ )
    {
      std::string sname = filenames[i];
      std::cout << "Source file " << i-1 << ": " << sname << std::endl;
      if ( tname == sname )
	{
	  // There's probably a million better ways of determining if these are the same
	  std::cout << "Source file is same name as target file, skipping it" << std::endl;
	  continue;
	}
      TFile* Source = TFile::Open( sname.c_str() );
      if ( ! Source ) continue; // Let ROOT print an error message about this condition
      FileList->Add(Source);
    }

  return;
}

void MergeRootfile( TDirectory *target, TList *sourcelist ) {

  //  cout << "Target path: " << target->GetPath() << endl;
  TString path( (char*)strstr( target->GetPath(), ":" ) );
  path.Remove( 0, 2 );

  TFile *first_source = (TFile*)sourcelist->First();
  first_source->cd( path );
  TDirectory *current_sourcedir = gDirectory;

  // loop over all keys in this directory
  TChain *globChain = 0;
  TIter nextkey( current_sourcedir->GetListOfKeys() );
  TKey *key, *oldkey=0;
  //gain time, do not add the objects in the list in memory
  TH1::AddDirectory(kFALSE);
  
  while ( (key = (TKey*)nextkey())) {
    
    //keep only the highest cycle number for each key
    if (oldkey && !strcmp(oldkey->GetName(),key->GetName())) continue;
    
    // read object from first source file
    first_source->cd( path );
    TObject *obj = key->ReadObj();

    std::cout << "Merging object " << obj->GetName() << std::endl;
    
    if ( obj->IsA()->InheritsFrom( "TH1" ) )
      {
	// descendant of TH1 -> merge it
	
	//      cout << "Merging histogram " << obj->GetName() << endl;
	TH1 *h1 = (TH1*)obj;
	
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
	    delete h2;
	  }
	  
	  nextsource = (TFile*)sourcelist->After( nextsource );
	}
      }
    else if ( obj->IsA()->InheritsFrom( "TTree" ) ) {
      
      // loop over all source files create a chain of Trees "globChain"
      if (!noTrees) {
	TString obj_name;
	if (path.Length()) {
	  obj_name = path + "/" + obj->GetName();
	} else {
	  obj_name = obj->GetName();
	}
	
	globChain = new TChain(obj_name);
        globChain->SetMaxTreeSize(100000000000LL); // set max size to ~100 GB
	globChain->Add(first_source->GetName());
	TFile *nextsource = (TFile*)sourcelist->After( first_source );
	//      const char* file_name = nextsource->GetName();
	// cout << "file name  " << file_name << endl;
	while ( nextsource ) {     	  
	  globChain->Add(nextsource->GetName());
	  nextsource = (TFile*)sourcelist->After( nextsource );
	}
      }
      
    } else if ( obj->IsA()->InheritsFrom( "TDirectory" ) ) {
      // it's a subdirectory
      
      std::cout << "Found subdirectory " << obj->GetName() << std::endl;
      
      // create a new subdir of same name and title in the target file
      target->cd();
      TDirectory *newdir = target->mkdir( obj->GetName(), obj->GetTitle() );
      
      // newdir is now the starting point of another round of merging
      // newdir still knows its depth within the target file via
      // GetPath(), so we can still figure out where we are in the recursion
      MergeRootfile( newdir, sourcelist );
      
    } else {
      
      // object is of no type that we know or can handle
      std::cout << "Unknown object type, name: "
	   << obj->GetName() << " title: " << obj->GetTitle() << std::endl;
    }
    
    // now write the merged histogram (which is "in" obj) to the target file
    // note that this will just store obj in the current directory level,
    // which is not persistent until the complete directory itself is stored
    // by "target->Write()" below
    if ( obj ) {
      target->cd();
      
      //!!if the object is a tree, it is stored in globChain...
      if(obj->IsA()->InheritsFrom( "TTree" )) {
	if (!noTrees) 
	  {
	    assert(globChain != NULL);
	    globChain->Merge(target->GetFile(),0,"keep");
	  }
      } else {
	obj->Write( key->GetName() );
      }
    }
    oldkey = key;

    delete obj;
    
  } // while ( ( TKey *key = (TKey*)nextkey() ) )
  
  // save modifications to target file
  target->SaveSelf(kTRUE);
  
}
