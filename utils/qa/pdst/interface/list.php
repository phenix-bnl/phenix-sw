<?php

$output = "";
if ($_POST['runlist'])
{
  $output = $_POST['runlist'];
}

header("Content-type: text/plain");

$output = strip_tags($output);

print($output);

?>