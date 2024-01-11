#define nt_emc_cxx
#include "nt_emc.hh"

void nt_emc::Loop()
{
//   In a Root session, you can do:
//      Root > .L nt_emc.C
//      Root > nt_emc t
//      Root > t.GetEntry(12); // Fill t data members with entry number 12
//      Root > t.Show();       // Show values of entry 12
//      Root > t.Show(16);     // Read and show values of entry 16
//      Root > t.Loop();       // Loop on all entries
//

//     This is the loop skeleton
//       To read only selected branches, Insert statements like:
// METHOD1:
//    fTree->SetBranchStatus("*",0);  // disable all branches
//    fTree->SetBranchStatus("branchname",1);  // activate branchname
// METHOD2: replace line
//    fTree->GetEntry(i);  // read all branches
//by  b_branchname->GetEntry(i); //read only this branch
   if (fTree == 0) return;

   Int_t nentries = Int_t(fTree->GetEntries());

   Int_t nbytes = 0, nb = 0;
   for (Int_t i=0; i<nentries;i++) {
      if (LoadTree(i) < 0) break;
      nb = fTree->GetEntry(i);   nbytes += nb;
   }
}
