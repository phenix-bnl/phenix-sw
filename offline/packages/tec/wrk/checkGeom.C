void checkGeom() {

// Load shared libraries
//  gROOT->Macro("loadlibraries.C");

   int runnumber = 76050;

// Create address object
   TecAddressObject* TAO = new TecAddressObject();
   cout << TAO->getName() << " created." << endl;
   //TAO->setDebugLevel(5);
   TAO->setRunNumber(runnumber);
   TAO->Fetch();
// Create geometry object
   TecGeometryObject* TGO = new TecGeometryObject();
   cout << TGO->getName() << " created." << endl;
   //TGO->setDebugLevel(5);
   TGO->setRunNumber(runnumber);
   TGO->Fetch();

//   PHTimeStamp Ttmp1 = TGO->getStartValidity();
//   Ttmp1.print(); cout << endl;
//   PHTimeStamp Ttmp2 = TGO->getEndValidity();
//   Ttmp2.print(); cout << endl;

int TECPACKETS=320;
int TECCHANNELS=32;
     for (int iFEM = 0;iFEM < TECPACKETS;iFEM++)
        {
          // check masked packets
          TAO->setHard(iFEM + 5001, 0);
          if (iFEM + 5001 != TAO->getPacketID())
            continue;   // masked in Database
          int iINDEX = TAO->getIndex();
          int iSIDE = TAO->getSide();
          if (!(TGO->isActivePlane(iINDEX)))
            continue;
          // to draw activeted planes and FEMs in tec view
          for (int ic = 0; ic < TECCHANNELS; ic += 8)
            {
              TAO->setHard(iFEM + 5001, ic);
              int pl = TAO->getPlane();
              int iWIRE = TAO->getWire();
                float x = TGO->getGlobalX(iINDEX, iWIRE);
                float y = TGO->getGlobalY(iINDEX, iWIRE);
                if(pl==0 && iWIRE>414) cout << iFEM << " " << iINDEX << " " << iWIRE << endl;
                if(pl==1 && iWIRE>414) cout << iFEM << " " << iINDEX << " " << iWIRE << endl;
                if(pl==2 && iWIRE>436) cout << iFEM << " " << iINDEX << " " << iWIRE << endl;
                if(pl==3 && iWIRE>447) cout << iFEM << " " << iINDEX << " " << iWIRE << endl;
                if(pl==4 && iWIRE>446) cout << iFEM << " " << iINDEX << " " << iWIRE << endl;
                if(pl==5 && iWIRE>468) cout << iFEM << " " << iINDEX << " " << iWIRE << endl;
            }
        }


}

