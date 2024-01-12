 void ExampleFVTX()
{
  gSystem->Load("libmuonqa");

  MuonQAAnalyzer* mQA = MuonQAAnalyzer::getInstance(); // MuonQAAnalyzer is a singleton

  SubsystemQA* qaRoot = new SubsystemQA("qaRoot");
  qaRoot->addPathsByRegex("/phenix/u/keyaaron/Run13QAFiles/", // Base path to search in
                          ".*/qaRoot$" // Regex to match to desired directories
                          );
  qaRoot->printPaths();
  
  mQA->addSubsystem(qaRoot);
  
  qaRoot->addHistoGroup("FVTXHits" , // Name of the group
                        "FVTX_hit_[0-1]_[0-1]" // Regex to identify the histograms
                        );
  
  qaRoot->addObservable("FVTXHits", // Specify the Histo/HistoGroup
                        "HitsPerEvent", // Give the observable a name
                        MuonQA::ENTRIES, // Which Observable I want
                        MuonQA::SUM, // Which summaries to save
                        200, 325 // Shortcut for specifying acceptable range for all runs
                        );

  qaRoot->addObservable("FVTXHits", // Specify the Histo/HistoGroup
                        "MissingWedges", // Give the observable a name
                        MuonQA::ACTIVEWEDGE, // Which Observable I want
                        MuonQA::SUM, // Which summaries to save
                        0, 40
                        );
  
  qaRoot->addHistoGroup("FVTXCoords" , // Name of the group
                        "FVTX_coord_[0-1]_[0-1]" // Regex to identify the histograms
                        );

  qaRoot->addObservable("FVTXCoords", // Specify the Histo/HistoGroup
                        "CoordsPerEvent",
                        MuonQA::ENTRIES, // Which Observable I want
                        MuonQA::SUM, // Which summaries to save
                        70, 140
                        );

  // Alternative interface method
  HistoGroupQA* coordSizeGroup = new HistoGroupQA("FVTXCoordSize",
                                                  "FVTX_coord_size_[0-1]_[0-1]");
  qaRoot->addHistogram(coordSizeGroup);
  ObservableQABase* avgCoordSize = new MeanQA("AvgCoordSize", 1, MuonQA::AVERAGE);
  avgCoordSize->setBounds(2, 2.5);  
      
  coordSizeGroup->addObservable(avgCoordSize);

  qaRoot->addHistoGroup("FVTXTracks",
                        "FVTX_trk_nhits_chi2_[0-1]_[0-1]");

  qaRoot->addObservable("FVTXTracks",
                        "AvgHitsPerTrack",
                        MuonQA::MEAN,
                        MuonQA::AVERAGE,
                        2.875, 3.125
                        );

  // Another alternative method for more control over the ObservableQA instantiation
  ObservableQABase* avgChi2 = new MeanQA("AvgChi2PerTrack", 2, // Take the mean along the Y axis
                                         MuonQA::AVERAGE
                                         );

  avgChi2->setBounds(3.125, 3.625);        
  qaRoot->addObservable("FVTXTracks",avgChi2);

  qaRoot->addHistoGroup("FVTXTrackDist",
                        "FVTX_trk_phi_theta_[0-1]");

  qaRoot->addObservable("FVTXTrackDist",
                        "TracksPerEvent",
                        MuonQA::ENTRIES,
                        MuonQA::SUM,
                        8, 20
                        );

  qaRoot->addObservable("FVTXTrackDist",
                        "PeakThetaTrackDist",
                        MuonQA::PEAK,
                        MuonQA::AVERAGE
                        // If you don't specify bounds then it is ignored for the good run list
                        );

  qaRoot->addHistoGroup("MuTr" , // Name of the group
                        "MutTrackMom.*" // Regex to identify the histograms
                        );

  qaRoot->addObservable("MuTr",
                        "NMuTrTracks",
                        MuonQA::ENTRIES,
                        MuonQA::SUM
                        );

  qaRoot->addObservable("MuTr",
                        "AverageTrackMom",
                        MuonQA::MEAN,
                        MuonQA::AVERAGE,
                        2.5, 4.0                        
                        );     
  
  mQA->processQA();
  mQA->outputGoodRunList();
}
