<?php header("Cache-control: private"); ?>

<html>
<LINK REL=stylesheet HREF="style.css" TYPE="text/css">

<SCRIPT>
<!--

// Function to capture only allowed chars 
function AllowChar(event) {
  //var strValidChars = "0123456789.";
  //var strChar;
  //var KeyTyped = String.fromCharCode(event.keyCode).toString();

  //var blnResult = true;

  //if (strValidChars.indexOf(KeyTyped)==-1) blnResult = false;

//for (i=0;i< strString.length && blnresult == true ; i++) {
//	strChar = strString.charAt(i);
//	if (strValidChars.indexOf(strChar) == -1) blnResult = false;
//	}

return blnResult;
}
// -->
</SCRIPT>

<body>

<div id="topblock">

<div style="">
<table width="100%" cellpadding="0" cellspacing="0">
<tr>
        <td width="200px">
        <img src="phenix_qc_logo_1.png" border=0 alt="Phenix Quality Control">
        </td>
     	<td valign=bottom>
        <div id="navbar">
        <table>
	<tr>
		<td><a href="index.php">Run Selector</a></td>
        	<td><a href="runlists.php" style="color: #1E90FF">Runlists</a></td>
        	<td><a href="update.php">Update Parameters</a></td>
                <td><a href="tutorial.html">Tutorial</a></td>
                <td><a href="about.php">About</a></td>
	</tr>
        </table>
        </div>
        </td>
</tr>
</table>
</div>


</div>

<div align="center">
<div style="height: 460px">

<?php


//$today = getdate();
//$month = $today[month];
//$weekday = $today[weekday];
//$mday = $today[mday];
//$year = $today[year];
//$hour = $today[hours];
//$min = str_pad($today[minutes],2,STR_PAD_LEFT);
//$sec = str_pad($today[seconds],2,STR_PAD_LEFT);

?>
</div>
</div>
<div style="border-top: 1px solid #333333 ; text-align: center ; color: blue">
<a href="mailto:mccumber@grad.physics.sunysb.edu" style="text-decoration: none">Email</a>
</div>
</body>
</html>

