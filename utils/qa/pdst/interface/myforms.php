<?php
require_once('myfunc.php');

//==========================================================
//  This function creates the form on the Plot Paramter area
//==========================================================

function get_param_plot_form($conn) {

  // Set default values
  $tag="";
  $par="";
  $paramlow=-9999999;
  $paramhigh=9999999;
  $runmin=0;
  $runmax=9999999;
  $shouldTally="no";

  // Reset to posted values if they exist
  if ($_POST['plotTabPostExists'])
    {
      $paramlow=$_POST['paramlow'];
      $paramhigh=$_POST['paramhigh'];
      $runmin=$_POST['runmin'];
      $runmax=$_POST['runmax'];
      $tag=$_POST['tag'];
      $par=$_POST['par'];
      $shouldTally=$_POST['shouldTally'];
    }

  echo "<form id=\"form1\" name=\"form1\" method=\"post\" action=\"index.php\">

        <input type=\"hidden\" name=\"tabname\" value=\"plottab\">
        <input type=\"hidden\" name=\"plotTabPostExists\" value=1>

	<table align=\"center\">
	<caption> Makes Lists and Plots for a Single Parameter </caption>
	<tr>
		<td>Parameter</td>
                <td></td>
		<td align=\"center\">Lowest</td>
		<td>Highest</td>
	</tr>
	<tr>
		<td>"; 
  echo "<select name=\"par\">";
  $parlist = get_parameter_list($conn);
  if ($par != "")
    {
      $parlist = str_replace("SELECTED>",">",$parlist);
      $parlist = str_replace("VALUE=\"$par\">","VALUE=\"$par\" SELECTED>",$parlist);
    }
  echo "$parlist";
  echo "</select>";
  
  echo "

                </td>
                <td></td>
		<td align=\"center\"><input type=\"text\" name=\"paramlow\" size=\"7\" value=$paramlow></td>
		<td><input type=\"text\" name=\"paramhigh\" size=\"7\" value=$paramhigh></td>
	</tr>
        <tr>
                <td align>Production Tag</td>
                <td></td>
                <td></td>
                <td></td>
        </tr>
        <tr>
                <td><select name=\"tag\">";
  $taglist=get_tag_list($conn);
  
  //Replace previously selected tag with tag SELECTED
  if($tag != "")
    {
      $taglist = str_replace("SELECTED>",">",$taglist);
      $taglist = str_replace("\"$tag\">","\"$tag\" SELECTED>",$taglist);
    }

  echo "$taglist";
  echo "         </select>               
                </td>
                <td></td>
                <td></td>
                <td></td>
        </tr>
	<tr>";
  echo str_replace("\"$shouldTally\">","\"$shouldTally\" CHECKED>",
               "<td>Tally BBC Events? (slow) <label>Yes<input type=\"radio\" name=\"shouldTally\" value=\"yes\"></label><label>No<input type=\"radio\" name=\"shouldTally\" value=\"no\"></label></td>");
  echo "	<td align=\"right\">Run Number</td>
		<td align=\"center\"><input type=\"text\" name=\"runmin\" size=\"7\" value=$runmin></td>
		<td><input type=\"text\" name=\"runmax\" size=\"7\" value=$runmax></td>
	</tr>
        <tr>
                <td></td>
                <td></td>
                <td></td>
        </tr>
	<tr>
		<td align=\"center\" colspan=\"4\"><input type=\"submit\" name=\"makePlot\" value=\"Plot\"> &nbsp &nbsp &nbsp &nbsp
                List Runs: &nbsp
                <input type=\"submit\" name=\"makeList\" value=\"Passing\">
                <input type=\"submit\" name=\"makeList\" value=\"Failing\">
                </td>
	</tr>
	</form>
	</table>";

}


//===================================================================
// This function creates the parameter selection form in the Multiple
// Parameter Cut area
//===================================================================

function get_param_select_form($conn) {

  //Set default parameter ranges if they don't exist
  $tag="";
  $numcuts=3;
  $maxnumcuts=50;
  if ($_POST['mparTabPostExists'])
    {
      $numcuts = $_POST['numcuts'];
    }
  for ($cut=1;$cut<=$numcuts;$cut++)
    {
      $mpar[$cut]=" ";
      $mparamlow[$cut]=-9999999;
      $mparamhigh[$cut]=9999999;
    }
  $runmin=0;
  $runmax=9999999;

  // if logic is ever changed, always use posted value
  $changedLogic="no";
  if ($_POST['changedLogic'] == "yes")
    {
      $changedLogic = "yes";
    }

  // Enters default full intersetion logic statement
  // (aka. a good run must pass all statements)

  $defaultLogic = "";
  for($cut=1;$cut<$numcuts;$cut++)
    {
      $defaultLogic = $defaultLogic."(C$cut)&";
    }
  $defaultLogic = $defaultLogic."(C$numcuts)";

  if ($_POST['mparTabPostExists'])
    {
      $tag = $_POST['tag'];
      for($cut=1;$cut<=$numcuts;$cut++)
	{
	  $mpar[$cut] = $_POST['mpar'][$cut];
	  $mparamlow[$cut]=$_POST['mparamlow'][$cut];
	  if ($mparamlow[$cut] == "") $mparamlow[$cut] = -9999999;
	  $mparamhigh[$cut]=$_POST['mparamhigh'][$cut];
	  if ($mparamhigh[$cut] == "") $mparamhigh[$cut] = 9999999;
	}
      $runmin=$_POST['runmin'];
      $runmax=$_POST['runmax'];
      //print("<br>Have I changed the logic??".$_POST['changedLogic']);
      if ($_POST['changedLogic'] == "yes")
	{
	  // Use the post value if user changed logic
	  $defaultLogic = $_POST['mparamlogicstring'];
	}
    }

  echo "
	        <form id=\"form2\" name=\"form2\" method=\"post\" action=\"index.php\">

                <input type=\"hidden\" name=\"tabname\" value=\"mparamtab\">
                <input type=\"hidden\" name=\"mparTabPostExists\" value=1>
                <input type=\"hidden\" name=\"shouldTally\" value=\"no\">
                <input type=\"hidden\" name=\"changedLogic\" value=$changedLogic>

		<table align=\"center\">
		<caption> Makes Plots and Lists for Multiple Parameters </caption>
                <tr><td></td><td>

        Number of Cuts: 
	<select onChange=\"document.form2.submit();\" name=\"numcuts\">
	";
  
  for ($i=1;$i<=$maxnumcuts;$i++)
    {
      if ($i==$numcuts) echo "<option label=\"$i\" value=\"$i\" selected>$i</option>";
      else echo "<option label=\"$i\" value=\"$i\">$i</option>";
    }
  echo "	</select>

                </td></tr>
		<tr>
			<td></td>
			<td>Parameter(s)</td>
			<td>Lowest</td>
			<td>Highest</td>
                        <td></td>
		</tr>
	";

  $parlist = get_parameter_list($conn);
  for ($cut=1;$cut<=$numcuts;$cut++) {
    //dump out a parameter select form
    echo "
		<tr>
			<td>C$cut:</td>
			<td>";


    echo "<select name=\"mpar[$cut]\">";

    if ($mpar[$cut] != "")
      {
	$parlist = str_replace("SELECTED>",">",$parlist);
	$parlist = str_replace("\"$mpar[$cut]\">","\"$mpar[$cut]\" SELECTED>",$parlist);
      }

    echo "$parlist";
    echo "</select>";
    echo "
                        </td>
			<td><input type=\"text\" name=\"mparamlow[$cut]\" size=\"7\" value=$mparamlow[$cut]></td>
			<td><input type=\"text\" name=\"mparamhigh[$cut]\" size=\"7\" value=$mparamhigh[$cut]></td>
                        <td><input type=\"submit\" name=\"makeMParPlot[$cut]\" value=\"Plot\"></td>
		</tr>";
  } //end for loop that runs over cuts	
  echo "


		<tr>
			<td></td>
                        <td>Production Tag</td>
                        <td></td>
                        <td></td>
                        <td></td>
                </tr>
                <tr>
                        <td></td>
                        <td><select name=\"tag\">";
  $taglist = get_tag_list($conn);
  if($tag != "")
    {
      $taglist = str_replace("SELECTED>",">",$taglist);
      $taglist = str_replace("\"$tag\">","\"$tag\" SELECTED>",$taglist);
    }
  echo "$taglist";
  echo "                </select></td>
                        <td></td>
                        <td></td>
                        <td></td>
                </tr>
                <tr>
                        <td></td>
			<td align=\"right\">Run Number</td>
			<td><input type=\"text\" name=\"runmin\" size=\"7\" value=$runmin></td>
			<td><input type=\"text\" name=\"runmax\" size=\"7\" value=$runmax></td>
                        <td></td>
		</tr>
		<tr>
			<td>Logic:</td>
			<td colspan=\"4\">";


  // cut from next input: onKeyPress=\"return AllowLogicChars(event);\"

  echo "
                        <input type=\"text\" name=\"mparamlogicstring\" size=\"90\" onChange=\"document.form2.changedLogic.value='yes';\" value=$defaultLogic>
                        </td>
		</tr>
		<tr>
			<td align=\"center\" colspan=\"4\">
                        List: &nbsp &nbsp
                        <input type=\"submit\" name=\"makeMParList\" value=\"Passing\"> &nbsp
                        <input type=\"submit\" name=\"makeMParList\" value=\"Failing\">
                        </td>
		</tr>
		</form>
		</table>
	";
}


//==========================================================
//  This function creates the form in the SQL Statement area
//==========================================================

function get_SQL_form() {
echo "
	<form id=\"form3\" name=\"form3\" method=\"post\" action=\"index.php\">

        <input type=\"hidden\" name=\"tabname\" value=\"SQLtab\">
        <input type=\"hidden\" name=\"sqlTabPostExists\" value=1>

	<table align=\"center\">
	<caption> Passes an SQL Statement </caption>
	<tr></tr>
	<tr>
		
		<td><input type=\"text\" name=\"query\" size=\"90\"></td>
	</tr><tr>
		<td align=\"center\"><input type=\"submit\" name=\"makeSqlList\" value=\"Query\"></td>
	</tr>
	</form>
	</table>
";
}

?>

