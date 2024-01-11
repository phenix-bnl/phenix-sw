//
// Setup FEM Map for Year-1
//                          Akio Kiyomichi Apr.29, 2000
//
{
  //#include <string.h>
  // Loading PHOOL libraries
  gSystem->Load("libEvent.so");
  gSystem->Load("libphool.so");
  gSystem->Load("libWrappers.so");
  gSystem->Load("libPdbCal.so");
  gSystem->Load("libPhHistogramFactory.so");
  gSystem->Load("libuti.so");
  gSystem->Load("libdcm.so");
  gSystem->Load("libdgo.so");
  gSystem->Load("libphgeo.so");
  gSystem->Load("libPISARoot.so");
  gSystem->Load("libgea.so");

  // Loading subsystem libraries
  gSystem->Load("libtof.so");

  // TOF Detector Object
  TofAddressObject* TofAddress = new TofAddressObject();
  TofAddress->setDebugLevel(1);
  //
  Int_t i,j,k;
  Int_t n_board, firstboard, firstslat, tofslat;
  Int_t slatid, crate, slot, channel;
  //
  // Crate - 0  slot0-7:F32-95, slot8-15:G32-95
  crate = 0;
  n_board = 8;
  firstboard = 0;
  firstslat = 32;  // F32
  for(i = 0; i < n_board; i++){
    slot = i + firstboard;
    for(j = 0; j < 8; j++){
      tofslat = i*8 + j + firstslat;
      slatid = TofAddress->getSlatID("F",tofslat);
      TofAddress->setHardMap(slatid,crate,slot,slot,j, j+8);
    }
  }
  n_board = 8;
  firstboard = 8;
  firstslat = 32;  // G32
  for(i = 0; i < n_board; i++){
    slot = i + firstboard;
    for(j = 0; j < 8; j++){
      tofslat = i*8 + j + firstslat;
      slatid = TofAddress->getSlatID("G",tofslat);
      TofAddress->setHardMap(slatid,crate,slot,slot,j, j+8);
    }
  }
  // Crate - 1  slot0-7:H32-95, slot8-15:I32-95
  crate = 1;
  n_board = 8;
  firstboard = 0;
  firstslat = 32;  // H32
  for(i = 0; i < n_board; i++){
    slot = i + firstboard;
    for(j = 0; j < 8; j++){
      tofslat = i*8 + j + firstslat;
      slatid = TofAddress->getSlatID("H",tofslat);
      TofAddress->setHardMap(slatid,crate,slot,slot,j, j+8);
    }
  }
  n_board = 8;
  firstboard = 8;
  firstslat = 32; // I32
  for(i = 0; i < n_board; i++){
    slot = i + firstboard;
    for(j = 0; j < 8; j++){
      tofslat = i*8 + j + firstslat;
      slatid = TofAddress->getSlatID("I",tofslat);
      TofAddress->setHardMap(slatid,crate,slot,slot,j, j+8);
    }
  }
  // Crate - 2  slot0-3:F00-31,   slot4-7:G00-31, 
  //           slot8-11:H00-31, slot12-15:I00-31
  crate = 2;
  n_board = 4;
  firstboard = 0;
  firstslat = 0;  // F00
  for(i = 0; i < n_board; i++){
    slot = i + firstboard;
    for(j = 0; j < 8; j++){
      tofslat = i*8 + j + firstslat;
      slatid = TofAddress->getSlatID("F",tofslat);
      TofAddress->setHardMap(slatid,crate,slot,slot,j, j+8);
    }
  }
  n_board = 4;
  firstboard = 4;
  firstslat = 0;  // G00
  for(i = 0; i < n_board; i++){
    slot = i + firstboard;
    for(j = 0; j < 8; j++){
      tofslat = i*8 + j + firstslat;
      slatid = TofAddress->getSlatID("G",tofslat);
      TofAddress->setHardMap(slatid,crate,slot,slot,j, j+8);
    }
  }
  n_board = 4;
  firstboard = 8;
  firstslat = 0;  // H00
  for(i = 0; i < n_board; i++){
    slot = i + firstboard;
    for(j = 0; j < 8; j++){
      tofslat = i*8 + j + firstslat;
      slatid = TofAddress->getSlatID("H",tofslat);
      TofAddress->setHardMap(slatid,crate,slot,slot,j, j+8);
    }
  }
  n_board = 4;
  firstboard = 12;
  firstslat = 0;  // I00
  for(i = 0; i < n_board; i++){
    slot = i + firstboard;
    for(j = 0; j < 8; j++){
      tofslat = i*8 + j + firstslat;
      slatid = TofAddress->getSlatID("I",tofslat);
      TofAddress->setHardMap(slatid,crate,slot,slot,j, j+8);
    }
  }
  // Crate - 3 slot0-11: J00-95
  crate = 3;
  n_board = 12;
  firstboard = 0;
  firstslat = 0;  // J00
  for(i = 0; i<n_board; i++){
    slot = i + firstboard;
    for(j = 0; j < 8; j++){
      tofslat = i*8 + j + firstslat;
      slatid = TofAddress->getSlatID("J",tofslat);
      TofAddress->setHardMap(slatid,crate,slot,slot,j, j+8);
    }
  }
  // Crate - 4  slot0-7:C32-95, slot8-15:D32-95
  crate = 4;
  n_board = 8;
  firstboard = 0;
  firstslat = 32;  // C32
  for(i = 0; i < n_board; i++){
    slot = i + firstboard;
    for(j = 0; j < 8; j++){
      tofslat = i*8 + j + firstslat;
      slatid = TofAddress->getSlatID("C",tofslat);
      TofAddress->setHardMap(slatid,crate,slot,slot,j, j+8);
    }
  }
  n_board = 8;
  firstboard = 8;
  firstslat = 32;  // D32
  for(i = 0; i < n_board; i++){
    slot = i + firstboard;
    for(j = 0; j < 8; j++){
      tofslat = i*8 + j + firstslat;
      slatid = TofAddress->getSlatID("D",tofslat);
      TofAddress->setHardMap(slatid,crate,slot,slot,j, j+8);
    }
  }
  // Crate - 5  slot1-8:A32-95, slot9-15,0:B32-95
  crate = 5;
  n_board = 8;
  firstboard = 0;
  firstslat = 32;  // A32
  for(i = 0; i < n_board; i++){
    slot = i + firstboard;
    for(j = 0; j < 8; j++){
      tofslat = i*8 + j + firstslat;
      slatid = TofAddress->getSlatID("A",tofslat);
      TofAddress->setHardMap(slatid,crate,slot,slot,j, j+8);
    }
  }
  n_board = 8;
  firstboard = 8;
  firstslat = 32;  // B32
  for(i = 0; i < n_board; i++){
    slot = i + firstboard;
    for(j = 0; j < 8; j++){
      tofslat = i*8 + j + firstslat;
      slatid = TofAddress->getSlatID("B",tofslat);
      TofAddress->setHardMap(slatid,crate,slot,slot,j, j+8);
    }
  }
  // Crate - 6  slot0-3:A00-31,   slot4-7:B00-31, 
  //           slot8-11:C00-31, slot12-15:D00-31
  crate = 6;
  n_board = 4;
  firstboard = 0;
  firstslat = 0;  // A00
  for(i = 0; i < n_board; i++){
    slot = i + firstboard;
    for(j = 0; j < 8; j++){
      tofslat = i*8 + j + firstslat;
      slatid = TofAddress->getSlatID("A",tofslat);
      TofAddress->setHardMap(slatid,crate,slot,slot,j, j+8);
    }
  }
  n_board = 4;
  firstboard = 4;
  firstslat = 0; // B00
  for(i = 0; i < n_board; i++){
    slot = i + firstboard;
    for(j = 0; j < 8; j++){
      tofslat = i*8 + j + firstslat;
      slatid = TofAddress->getSlatID("B",tofslat);
      TofAddress->setHardMap(slatid,crate,slot,slot,j, j+8);
    }
  }
  n_board = 4;
  firstboard = 8;
  firstslat = 0;  // C00
  for(i = 0; i < n_board; i++){
    slot = i + firstboard;
    for(j = 0; j < 8; j++){
      tofslat = i*8 + j + firstslat;
      slatid = TofAddress->getSlatID("C",tofslat);
      TofAddress->setHardMap(slatid,crate,slot,slot,j, j+8);
    }
  }
  n_board = 4;
  firstboard = 12;
  firstslat = 0; // D00
  for(i = 0; i < n_board; i++){
    slot = i + firstboard;
    for(j = 0; j < 8; j++){
      tofslat = i*8 + j + firstslat;
      slatid = TofAddress->getSlatID("D",tofslat);
      TofAddress->setHardMap(slatid,crate,slot,slot,j, j+8);
    }
  }
  // Crate - 7 slot0-11: E00-95
  crate = 7;
  n_board = 12;
  firstboard = 0;
  firstslat = 0;  // E00
  for(i = 0; i < n_board; i++){
    slot = i + firstboard;
    for(j = 0; j < 8; j++){
      tofslat = i*8 + j + firstslat;
      slatid = TofAddress->getSlatID("E",tofslat);
      TofAddress->setHardMap(slatid,crate,slot,slot,j, j+8);
    }
  }

  TofAddress->writeToFile("toffemmap.txt.year1");
}
