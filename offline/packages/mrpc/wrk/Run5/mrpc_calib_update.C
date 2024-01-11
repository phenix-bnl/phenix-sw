void mrpc_calib_update()
{
  // __________________________________________________________
  //
  //  This macro does the DB update for MRPC calib.
  //  
  //  Run 146651: the first run for MRPC (TOF.W) time-in.
  //  (2005-01-06 04:51:34). 
  //  The end run is not specified, i.e. put the "FarFuture"
  //  EndTime by default. -- T.Chujo (1/31/2005)
  // __________________________________________________________

  gSystem->Load("libPgCalInstance.so");
  gSystem->Load("/phenix/data20/chujo/MRPC/offline/install/lib/libmrpc.so");

  // === MRPC Calib ===
  MrpcCalib* calib = new MrpcCalib();
  calib->set_debug(1);

  // === Fetch from file ===
  calib->fetchTvcConv("./calibpar/mrpc_tvcconv.txt");
  calib->fetchTvcPede("./calibpar/mrpc_tvcpede.txt");
  calib->fetchQvcConv("./calibpar/mrpc_qvcconv.txt");
  calib->fetchQvcPede("./calibpar/mrpc_qvcpede.txt");
  calib->fetchGlobalT0("./calibpar/mrpc_globalt0.txt");
  calib->fetchSlatT0("./calibpar/mrpc_slatt0.txt");
  calib->fetchSlewParA("./calibpar/mrpc_slewpara.txt");
  calib->fetchSlewParB("./calibpar/mrpc_slewparb.txt");
  calib->fetchYoffset("./calibpar/mrpc_yoffset.txt");
  calib->fetchVelocity("./calibpar/mrpc_velocity.txt");
  
  //calib->print(10);

  // === DB update (Run 146651) ===

  int run = 146651;

  calib->update(run); // update at once.

  /*
  calib->updateTvcConv(run);
  calib->updateTvcPede(run);
  calib->updateQvcConv(run);
  calib->updateQvcPede(run);
  calib->updateGlobalT0(run);
  calib->updateSlatT0(run);
  calib->updateSlewParA(run);
  calib->updateSlewParB(run);
  calib->updateYoffset(run);
  calib->updateVelocity(run);
  */

  // === Write to file for check ===
  calib->writeTvcConv("./calibpar/out/mrpc_tvcconv.out");
  calib->writeTvcPede("./calibpar/out/mrpc_tvcpede.out");
  calib->writeQvcConv("./calibpar/out/mrpc_qvcconv.out");
  calib->writeQvcPede("./calibpar/out/mrpc_qvcpede.out");
  calib->writeGlobalT0("./calibpar/out/mrpc_globalt0.out");
  calib->writeSlatT0("./calibpar/out/mrpc_slatt0.out");
  calib->writeSlewParA("./calibpar/out/mrpc_slewpara.out");
  calib->writeSlewParB("./calibpar/out/mrpc_slewparb.out");
  calib->writeYoffset("./calibpar/out/mrpc_yoffset.out");
  calib->writeVelocity("./calibpar/out/mrpc_velocity.out");

}
