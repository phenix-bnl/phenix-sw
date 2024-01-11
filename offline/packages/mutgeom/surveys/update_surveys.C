void update_surveys()
{
printf("Loading PHOOL libraries\n");
  gSystem->Load("libfun4all.so");


  /*  Surveys are used by a few MutGeomObjects: MutOctant, 
      MutPlane, and MutStation.  The good news is that all of 
      these database files, except or the arm surveys, have 
      already been committed to the database and don't need to be 
      modified again.

      The afs location of the original files is
      /afs/rhic.bnl.gov/phenix/PHENIX_LIB/calibration/mutr_txt/

      Arm surveys (or "octant surveys" as I originally called them) 
      contain points at several places on each octant.  These points 
      are in cartesian coordinates in the PHENIX global reference 
      frame.  Pictures are needed to fully understand the scheme.
      For the MutOctantPositions.dat file, three of the survey 
      points are used to specify the octnat planes.  For station 1, 
      there are only three survey points for each octant.  
      There are more points for stations 2 and 3, so all points can be 
      fitted, and the fit values used in MutOctantPositions.dat.

      Station 3 strips were also surveyed and, although the strip 
      surveys themselves don't need updating, the halfoctants do.  As 
      these strips were being incorporated into the geometry, it became 
      clear that three survey point from above weren't enough.  So more 
      points are needed for station 3 and are stored (and called) 
      separatly in St3OctantSurveys.dat.

      The arms do not need to be created from the database in order 
      to store the new file!  It saves time not to call the database 
      arm constructor via  MutArm(armNum, timeStamp)

  */

  //used for 
  PHTimeStamp Tstart(2001,6,1,0,0,0,0);
  PHTimeStamp Tstop;
  Tstop.setToFarFuture();

  char *descrip = "2002 survey";
  char *fileName = "MutOctantPositions.run3.dat";
  //char *fileName = "St3OctantSurveys.dat";

  PdbBankID bankID = 0;
  MutArm *s = new MutArm(0);
  MutArm *n = new MutArm(1);

  //confirmation that everything is going well...
  s->print();
  n->print();
  
  if(n->updateOctantSurvey(Tstart, Tstop, 0, descrip, fileName)) cout<<fileName<<" written to DB\n";
  if(s->updateOctantSurvey(Tstart, Tstop, 0, descrip, fileName)) cout<<fileName<<" written to DB\n";

  /*  You should not need to update strip positions again */
  //n->f_pMutStations[0]->updateStripPositions(Tstart, Tstop);
  //n->f_pMutStations[2]->updateStripPositions(Tstart, Tstop);
  //s->f_pMutStations[2]->updateStripPositions(Tstart, Tstop);

  /*  You WILL need to update station 3 half octant frame positions
      when a new survey is taken.
  */
  //n->f_pMutStations[2]->updateSt3HalfOctFrame(Tstart, Tstop, fileName);
  //s->f_pMutStations[2]->updateSt3HalfOctFrame(Tstart, Tstop);

  delete s;
  delete n;
}












