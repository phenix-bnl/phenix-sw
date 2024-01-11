void read_smmaskDB(void)
{
gSystem->Load("libPgCalInstance.so");
PdbBankManager* bm = PdbBankManager::instance();
PdbApplication* app = bm->getApplication();
app->startRead();
PdbBankID bankID;
bankID.setInternalValue(0);
char *nameDB = "calib.ertsmmask";
int run = 256000; 
PdbCalBank *b = bm->fetchBank("PdbErtSMMask", bankID, nameDB,run);

/*
// An alternative way when RunToTime isn't set
PHTimeStamp tFetch= PHTimeStamp(2008,11,9,0,0,0);
PdbCalBank *b = bm->fetchBank("PdbErtSMMask", bankID, nameDB,tFetch);
*/


 b->printHeader();
 PdbErtSMMask ertmask = (PdbErtSMMask &) b->getEntry(0);

/*
 for(int ii=0;ii<32;ii++)
   {
     cout << ii << " " << ertmask.Get(1,3,ii,0) << endl;;
   }
 cout << endl << endl;
*/

 ertmask.print();
 
}
