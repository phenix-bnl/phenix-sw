/*

 This macro will add histograms from a list of root files and write them
 to a target root file. The target file is newly created and must not be
 identical to one of the source files.

 Author: Sven A. Schmidt, sven.schmidt@cern.ch
 Date:   13.2.2001

 This code is based on the hadd.C example by Rene Brun and Dirk Geppert,
 which had a problem with directories more than one level deep.
 (see macro hadd_old.C for this previous implementation).
 
 The macro from Sven has been enhanced by 
    Anne-Sylvie Nicollerat <Anne-Sylvie.Nicollerat@cern.ch>
  to automatically add Trees (via a chain of trees).
 
 To use this macro, modify the file names in function hadd.
 
 NB: This macro is provided as a tutorial.
     Use $ROOTSYS/bin/hadd to merge many histogram files

*/

#include <list>
#include <string>   
//INCLUDECHECKER: Removed this line: #include <sys/types.h>
#include <sys/stat.h>
//INCLUDECHECKER: Removed this line: #include <unistd.h>
#include "TChain.h"
#include "TFile.h"
#include "TH1.h"
//INCLUDECHECKER: Removed this line: #include "TTree.h"
#include "TKey.h"
#include "THmulf.h"
#include <iostream>
#include <fstream>
#include <boost/scoped_ptr.hpp>

using namespace std;

int noTrees = 0;

void 
MergeRootfile(TDirectory *target, std::list<TFile*>& sourcelist)
{
  TString path((char*)strstr(target->GetPath(), ":"));
  path.Remove(0, 2);

  TFile *first_source = sourcelist.front();
  sourcelist.pop_front();
  first_source->cd(path);
  TDirectory *current_sourcedir = gDirectory;

  // loop over all keys in this directory
  TChain *globChain = 0;
  TIter nextkey(current_sourcedir->GetListOfKeys());
  //gain time (and memory), do not add the objects in the list in memory
  TH1::AddDirectory(kFALSE);

  while ( TKey* key = (TKey*)nextkey() )
    {
      // Read object from first source file
      first_source->cd(path);
      boost::scoped_ptr<TObject> obj(key->ReadObj());

      std::cout << "Merging object " << obj->GetName() << std::endl;

      if (obj->IsA()->InheritsFrom("TH1"))
        {
          // descendant of TH1 -> merge it

          TH1 *h1 = (TH1*)obj.get();

          // loop over all source files and add the content of the
          // corresponding histogram to the one pointed to by "h1"
	  std::list<TFile*>::const_iterator iter;
	  for (iter=sourcelist.begin(); iter!=sourcelist.end(); iter++)
	    {
              // make sure we are at the correct directory level by cd'ing to path
              (*iter)->cd(path);
              if ( TH1* h2 = (TH1*)gDirectory->Get(h1->GetName()) )
                {
                  h1->Add(h2);
                  delete h2; // don't know if this is necessary, i.e. if
                  // h2 is created by the call to gDirectory above.
                }

            }

        } // if Inherits from TH1

      else if ( !noTrees && obj->IsA()->InheritsFrom( "TTree" ) ) 
	{
	  // loop over all source files create a chain of Trees "globChain"
	  TString obj_name;
	  if (path.Length()) {
	    obj_name = path + "/" + obj->GetName();
	  } else {
	    obj_name = obj->GetName();
	  }
	  
	  globChain = new TChain(obj_name);
	  globChain->Add(first_source->GetName());
	  std::list<TFile*>::const_iterator iter;
	  for (iter=sourcelist.begin(); iter!=sourcelist.end(); iter++)
	    {
	      globChain->Add((*iter)->GetName());
	    }
	} // if Inherits from TTree

      else
        {
          // object is of no type that we know or can handle
          cout << "Unknown object type, name: "
	       << obj->GetName() << " title: " << obj->GetTitle() << endl;
	  continue;
        }

      // now write the merged histogram (which is "in" obj) to the target file
      // note that this will just store obj in the current directory level,
      // which is not persistent until the complete directory itself is stored
      // by "target->Write()" below
      if (obj)
        {
          target->cd();

          //!!if the object is a tree, it is stored in globChain...
          if (!noTrees && obj->IsA()->InheritsFrom("TTree"))
	    {
	      // Merge is used in haddPhenix, do we need it here?
	      assert(globChain != NULL);
	      globChain->Merge(target->GetFile(),32000,"keep"); // keep output file open!
	      //globChain->Write(key->GetName());
	    }
          else
	    {
	      obj->Write(key->GetName());
	    }
        }

    } // while ((TKey *key = (TKey*)nextkey()))

  // save modifications to target file
  target->Write();

}

int
main(int argc, char *argv[])
{
  list<TFile *> FileList;
  char buffer[256];
  if (argc != 3)
    {
      exit(1);
    }
  // First argument contains file with list of input files
  ifstream fin(argv[1]);
  if (!fin.is_open())
    {
      exit(1);
    }

  while (fin.getline(buffer, 256, '\n') || fin.gcount())
    {
      cout << "Source file: " << buffer << endl;
      TFile *f = new TFile(buffer);
      if ( f->IsOpen() ) FileList.push_back(f);
    }

  if ( ! FileList.size() ) 
    {
      std::cout << "WARNING: no input files specified" << std::endl;
      exit(1);
    }

  TFile* Target = 0;

  if (!(Target = TFile::Open(argv[2], "RECREATE")))
    {
      exit(1);
    }
  
  MergeRootfile(Target, FileList);

  delete Target;

  return 0;
}

