<?php
  list ($subsys, $parameter) = split("->",$SubsysPar);
  $subsystable = $subsys."qa";
  if ($option==1 && !strcmp($parameter,"all")) echo "It's possible plot just one parameter.<br>";

  if ($option==2 && !strcmp($parameter,"all")) echo "It's only possible select runs using a specific parameter.<br>";

  if($runmax<$runmin || $valmax<$valmin) echo "The range is not valid.<br>";

// selection of runs according range
if ($option==2 && $runmax>$runmin && $valmax>$valmin && strcmp($parameter,"all") && strcmp($subsys,"any"))
{
	require_once ('QAsummary.inc');
  $pdbcal = new postgres("calibrations");
  $res = $pdbcal->query("select distinct on (runnumber) runnumber from (select * from $subsystable where parname like '$parameter' and runnumber>=$runmin and runnumber<=$runmax and parameter+parerror > $valmin and parameter-parerror < $valmax and tag like '$tagsel' order by inserttime desc) as subsysselected");
  while ($line = $pdbcal->to_array($res))
    {
			if (selection($seltext, $line["runnumber"],$tagsel)) 
				echo $line["runnumber"]."<br>";
    } 
}

// Parameter x Run plot
if ($option==1 && $runmax>$runmin && $valmax>$valmin &&  strcmp($parameter,"all") && strcmp($subsys,"any"))
{
  require_once ('QAsummary.inc');
  require_once ('graph2d.class.php');
  $pdbcal = new postgres("calibrations");
  $res = $pdbcal->query("select distinct on (runnumber) runnumber,parameter,parerror from (select * from $subsystable where parname like '$parameter' and runnumber>=$runmin and runnumber<=$runmax and parameter+parerror > $valmin and parameter-parerror < $valmax and tag like '$tagsel' order by inserttime desc) as subsysselected");
  $runno = array();
  $Y = array();
  $Yerror = array();
  $i = 0;
  $Ymin = 99999.9;
  $Ymax = -99999.9;
  $runmin = 9999999999;
  $runmax = 0;
  $N_events = 0;
  while ($line = $pdbcal->to_array($res))
    {
      $runno[$i] = $line["runnumber"];
      $parval = $line["parameter"];
      if (!selection($seltext, $runno[$i],$tagsel)) continue;
      $N_events += n_events($runno[$i]);
      $Y[$i] = $parval;
      $Yerror[$i] = $line["parerror"];
      if ($Y[$i]-$Yerror[$i] < $Ymin) $Ymin = $Y[$i]-$Yerror[$i];
      if ($Y[$i]+$Yerror[$i] > $Ymax) $Ymax = $Y[$i]+$Yerror[$i];
      if ($runno[$i] < $runmin) $runmin = $runno[$i];
      if ($runno[$i] > $runmax) $runmax = $runno[$i];
      $parname = $parameter;
      $subsysname = $subsys;
      $i ++;
    }
  // selecting parameter range
  if ($valmin>$Ymin && $valmin<$Ymax) $Ymin=$valmin;
  if ($valmax<$Ymax && $valmax>$Ymin) $Ymax=$valmax;

// here I try to make a more convenient Y labels
  if (abs($Ymax)>1000 || abs($Ymin)>1000)
    {
      $Ymax /= 1000;
      $Ymin /= 1000;
      for ($i=0; $i<count($Y); $i++) {$Y[$i] /=1000; $Yerror[$i] /=1000;}
      $parname .= "  x10^3";
    }
  else
    {
      if (abs($Ymax)<0.9)
       {
          $Ymax *= 1000;
          $Ymin *= 1000;
          for ($i=0; $i<count($Y); $i++) {$Y[$i] *=1000; $Yerror[$i] *= 1000;}
         $parname .="  x10^-3";
       }
    }
  //------------------------------------------------------------------------------------------------

  $n=count($runno);
  $ystep = ($Ymax-$Ymin)/10;
  $xstep = ($runmax-$runmin)/5;

  $parplot=new graph2d ("jpeg", 1000, 350, 150, 20, 20, 40);
        
  $parplot->SetAxisScales ($runmin, $runmax, $xstep, 1, $Ymin, $Ymax, $ystep, 1);
  
  $parplot->SetGrid(TRUE, 0xA0, 0xA0, 0xA0);
  
  $parplot->SetTitle ($subsysname." - ".$N_events." BBCLL1 events");
  $parplot->SetXLegend ("RUN Number");
  $parplot->SetYLegend ($parname);
  
  $parplot->GraphCoord();
  $parplot->Draw($runno, $Y, $Yerror, 0xFF, 0, 0);
  
  $parplot->ImageOut("");
  
  $parplot->Close();
}

// Print parameter(s)
if ($option==3 && $runmax>=$runmin)
{
   require_once ('QAsummary.inc');
  $runmin = run2time($runmin);
  $runmax = run2time($runmax);
  $timeselect =sprintf("runnumber>=%d and runnumber<=%d", $runmin,$runmax);
  $pdbcal = new postgres("calibrations");
  if (strcmp($parameter,"all"))
    $res = $pdbcal->query("select distinct on (runnumber) runnumber,parameter,parerror,parname   from (select * from $subsystable where parname like '$parameter' and runnumber>=$runmin and runnumber<=$runmax and parameter+parerror > $valmin and parameter-parerror < $valmax and tag like '$tagsel' order by inserttime desc) as subsysselected order by runnumber asc");
  else
    $res = $pdbcal->query("select distinct on (runnumber,parname) runnumber,parameter,parerror,parname from (select * from $subsystable where runnumber>=$runmin and runnumber<=$runmax and tag like '$tagsel' order by inserttime desc) as subsysselected order by runnumber asc");

  $newruntime = 0;

  while ($line = $pdbcal->to_array($res))
    {
      if ($line["runnumber"]!=$newruntime)
	{
	  $newruntime = $line["runnumber"];
	  echo "</tbody></table><br>";
	  $runno = time2run($line["runnumber"]);
	  echo "<hr style=\"width: 100%; height: 2px;\">";
	  echo "<h2 style=\"text-align: center; font-family: times".
	    "new roman,times,serif; background-color: rgb(255, 255, 204); color: rgb(153, 51, 0);\">".
	    "RUN : ".
	    " with ".n_events($runno)." events</h2><br>";
	  echo "<table cellpadding=\"2\" cellspacing=\"2\" border=\"1\"".
	    "style=\"text-align: left; width: 65%;\"><tbody><tr align=\"center\">".
	    "<td style=\"vertical-align: top;\" colspan=\"2\">".
	    "<h3 style=\"background-color: rgb(255, 153, 0);\">";
	  echo $subsys."<br>";
	  echo "</h3></td></tr>";
	}
      echo "<tr><td style=\"vertical-align: top; font-weight:".
	"bold; color: rgb(51, 0, 51); background-color: rgb(255, 204, 153);\">";
      echo htmlentities($line["parname"])."</td>";
      echo "<td style=\"vertical-align: top;\">";
      echo $line["parameter"]." +/- ".$line["parerror"]."</td>";
      echo "</tr>";
    }
}

function selection($seltext, $runnumber, $tagsel)
{
	if (strlen($seltext)==0) return 1;
	require_once ('QAsummary.inc');
	$seltext = str_replace('&&', "and", $seltext);
	$seltext = str_replace('AND', "and", $seltext);
	$selList = explode("and", $seltext);
	for ($i=0; $i<count($selList); $i++)
		{
			$selList[$i] = str_replace("->","/t",$selList[$i]);
			$posop = 0;
			if (strrpos($selList[$i], "=")>$posop) {$posop =strrpos($selList[$i], "="); $op="=";}
			if (strrpos($selList[$i], ">")>$posop) {$posop = strrpos($selList[$i], ">");$op=">";}
			if (strrpos($selList[$i], "<")>$posop) {$posop = strrpos($selList[$i], "<");$op="<";}
			if (strrpos($selList[$i], ">=")>$posop) {$posop = strrpos($selList[$i], ">=");$op=">";}
			if (strrpos($selList[$i], "<=")>$posop) {$posop = strrpos($selList[$i], "<=");$op="<";}
			$selList[$i] = substr_replace($selList[$i], "/t", $posop, strlen($posop)-1);
			list ($subsysname, $parname, $valsel) = split('/t',$selList[$i]);
			$parnamesize = strlen($parname)-1;
			if ($parname[$parnamesize]==' ') $parname = substr_replace($parname, '', $parnamesize);
			$subsystable = $subsysname."qa";
			$parname = str_replace("*","%",$parname);
			if (!strcmp("=",$op)) $parcut = "parameter-parerror<$valsel and parameter+parerror>$valsel";
			if (!strcmp(">",$op)) $parcut = "parameter-parerror>$valsel";
			if (!strcmp("<",$op)) $parcut = "parameter+parerror<$valsel";
			$pdbcal = new postgres("calibrations");
			$res = $pdbcal->query("select $parcut as result from $subsystable where runnumber=$runnumber and parname like '$parname' and tag like '$tagsel' order by inserttime desc limit 1");
			$line = $pdbcal->to_array($res);
			$result = $line["result"];
			if (!strcmp($result,'f')) return 0;
		}
	return 1;
}

?>