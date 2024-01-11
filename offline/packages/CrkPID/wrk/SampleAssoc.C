InitRun()
{
  recoConsts *rc = recoConsts::instance();
  CreateNodeTree(topNode);
  int runnumber = rc->get_IntFlag("RUNNUMBER");
  if (rc->FlagExist("SIMULATIONFLAG"))
    {
      if (rc->get_IntFlag("SIMULATIONFLAG") >= 1)
        {
          runnumber = -1;
        }
    }
  cout << "RingReco using Run number " << runnumber << endl;
  crkpid = new CrkPID(runnumber);

  return 0;
}

process_event()
{

  // They are PID parameters.
  float R0,R1,R2,Rmin,Rmax;
  crkpid->GetPidParameters(R0,Rmin,Rmax,R1,R2);

  crkpid->SetBbcT0(TimeZero);
  crkpid->SetCrkHitFromTop(topNode);

  // Loop over all charged tracks and make the damned rings!!!
  for (unsigned int i = 0; i < PHTrack->get_PHNTrack(); i++)
    {
      // Some preceding codes here..


      // Getting CrkHit and reflected tracks
      PHPoint mr_cross;
      PHPoint pmt_cross;
      PHLine ref_rich_proj;

      CrkHitExtv1 crkhitext;

      crkpid->GetPMThit(crkhitext);
      crkpid->TrackOnMirrorAndPMT(rich_proj,ref_rich_proj,mr_cross,pmt_cross);

      // How to calculate distance between PMT and tracks
      numhits = crkhitext.get_CrkNhits();
      for( unsigned int j=0;j<numhits;j++){
         PHPoint pmt_pos(crkhitext.get_posX(j),
                         crkhitext.get_posY(j),
                         crkhitext.get_posZ(j));
 
         double r_cor = distanceLinePoint(ref_rich_proj, pmt_pos);

          if(r_cor < d_R2) {
            npmt1++;
            npe1 += npe;
            xcenter1 += (pmt_pos_cor.getX() * npe);
            ycenter1 += (pmt_pos_cor.getY() * npe);
            zcenter1 += (pmt_pos_cor.getZ() * npe);
            chisqr += (r_cor - R0)*(r_cor - R0)*npe;
          }
       }
       center1 = PHPoint(xcenter1/npe1,ycenter1/npe1,zcenter1/npe1);
    }
}

