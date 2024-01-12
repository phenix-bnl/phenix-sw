<?php
require_once('postgres.php');

function time2run($init)
{
  $run = new postgres("Phenix");
  $res = $run->query("select runnumber from run where brunixtime>=$init order by runnumber limit 1");
  $line =  $run->to_array($res);
  if ($run->num_rows($res)==1) return $line["runnumber"];
  if ($run->num_rows($res)==0)
    {
      $res = $run->query("select max(runnumber) from run");
      $line =  $run->to_array($res);
      return $line["runnumber"];
    }
}

function run2time($runno)
{
  $run = new postgres("Phenix");
  $res = $run->query("select brunixtime from run where runnumber>=$runno order by runnumber limit 1");
  $line =  $run->to_array($res);
  if ($run->num_rows($res)==1) return $line["brunixtime"];
  if ($run->num_rows($res)==0) return 3000000000;   // if it don't find any, set for a very far time
}

function n_events($runno)
{
  $run = new postgres("Phenix");
  $res = $run->query("select scalerupdateraw from trigger where runnumber=$runno");
  $line =  $run->to_array($res);
  return $line["scalerupdateraw"];
}

function list_parameters()
{
  $pdbcal = new postgres("calibrations");
	//  $res = $pdbcal->query("select description from qasummary group by description");
	$res = $pdbcal->query("select relname from pg_stat_user_tables where relname like '%qa'");
  echo "Item : <SELECT NAME=\"SubsysPar\"><OPTION VALUE=\"any\">Any</OPTION>";
  while ($line = $pdbcal->to_array($res))
    {
			$subsystable = $line["relname"];
			$subsysname = str_replace("qa","",$subsystable);
      echo "<OPTGROUP LABEL=\"".$subsysname."\">";
      echo "<OPTION VALUE=\"".$subsysname."->all\">".$subsysname."-> All Parameters</OPTION>";
      $res1 = $pdbcal->query("select parname from $subsystable group by parname");
      while ($line1 = $pdbcal->to_array($res1))
				{
					$parname = htmlentities($line1["parname"]);
					echo "<OPTION VALUE=\"".$subsysname."->".$parname."\">".
						$subsysname."->".$parname."</OPTION>";
				}
      echo "</OPTGROUP>";
    }
  echo "</SELECT>";
}

function list_tags()
{
  $pdbcal = new postgres("calibrations");
  echo "Tag : <SELECT NAME=\"tagsel\">";
  $res = $pdbcal->query("select tag from bbcqa group by tag");
  while ($line = $pdbcal->to_array($res))
    {
      $tag = $line["tag"];
			echo "<OPTION VALUE=\"".$tag."\">".$tag."</OPTION>";
		}
  $res = $pdbcal->query("select tag from mutrqa group by tag");
  while ($line = $pdbcal->to_array($res))
    {
      $tag = $line["tag"];
			echo "<OPTION VALUE=\"".$tag."\">".$tag."</OPTION>";
		}
  echo "</SELECT>";
}

?>
