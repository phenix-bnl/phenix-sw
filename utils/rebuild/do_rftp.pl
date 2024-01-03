#!/usr/local/bin/perl
$ARGV[0]=~/^(.*)(vol\w*)(\S*)$/;
$file=$2.$3;
`rftp <<EOF
lcd $1
mkdir $2
cd $2
put $file
quit
EOF
`;
