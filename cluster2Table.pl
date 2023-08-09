#!/usr/bin/perl

use strict;
use warnings;
use File::Find;
use File::Basename;
use Getopt::Long;

##Use File::Grep qw ( fgrep );

sub annotateAnova;
sub trimWhiteSpaces($);
sub trim($);
sub ltrim($);
sub rtrim($);
sub loadFiles($);
sub isWantedFile();
sub hasClusters($);

## default to TLRC space
my $space="TLRC";
## default to center of mass of the clusters
my $columns="1,2,3";
## default to center of mass cordinates
my $com=1;
## another option are the coordinates of the maximum intensity
my $mi=0;
# force creation of the cluster locations csv file even it already exists
my $force=0;
GetOptions ("space=s" => \$space,   # string
	    "com"     => \$com,     # flag
	    "mi"      => \$mi,      # flag
	    "force"    => \$force); # flag

$space=uc($space);

if ($space ne "TLRC" && $space ne "MNI" && $space ne "MNI_ANAT" ) {
  die "Unrecognized space: $space.\nSpace can only be one of: TLRC (default), MNI, or MNI_ANAT\n";
}

if ($mi) {
  $com=0;
  print "#####################################################\n";
  print "### Processing ** Maximum Intensity ** cordinates ###\n";
  print "#####################################################\n";
} else {
  print "##################################################\n";
  print "### Processing ** Center of Mass ** cordinates ###\n";
  print "##################################################\n";
}


if ($mi) {
  $columns="13,14,15";
}

print "Template space is $space\n";

my @groupDirs=();

if ( $#ARGV + 1 < 1 ) {
  die "You forgot to supply at least one directory containing group results\n";
} else {
  @groupDirs = @ARGV;
}

my @clusterFiles = ();

#loadFiles($dir); #call
#map { print "$_\n"; } @clusterFiles;


foreach my $groupDir ( @groupDirs ) {
  print "Processing clust files in $groupDir\n";
  #@clusterFiles = ();
  #loadFiles($groupDir);
  
  @clusterFiles =  glob( "$groupDir/clusters.table.*.txt" );

  foreach my $cf ( @clusterFiles ) {
    my $clusterFile = "$cf";
    print "Processing the clusters in " . basename($clusterFile) . " and writing ";

    my $clusterLocations = $clusterFile;
    $clusterLocations =~ s/clusters\.table\.(.*)\.txt/clusters.locations.$1.csv/;

    print basename($clusterLocations) . "\n";

    if ( -f $clusterFile ) {
      my $h=hasClusters($clusterFile);
      ##print"h is $h\n";
      if ( $h ) {
	#print "INFO: Found clusters in $clusterFile\n";
	if ( -f $clusterLocations  && $force ) {
	  unlink $clusterLocations;
	}
	## if clusterLocations exists and is older then the
	## clusterFile then delete the clusterLocaitons file
	if  ( -f  $clusterLocations && ((stat($clusterLocations))[9] < (stat($clusterFile))[9] ) ) {
	    unlink $clusterLocations;
	}

	if ( ! -f $clusterLocations ) {
	    open(OUT,">$clusterLocations") || die "Cannot write to file $clusterLocations: $!";
	  
	  my @TailarachLocations = annotateAnova($clusterFile);
	  {
	    local $, = ",";
	    print OUT @TailarachLocations;
	    print OUT "\n";
	  }
	} else {
	  print "WARNING: The cluster locations file $clusterLocations already exists. Skipping.\n";
	}
      }
    }		       ## end of if ( -f $roiFile && -f $clusterFile )
    else {
      if ( ! -f $clusterFile ) {
	print "ERROR: The cluster list file $clusterFile does not exist\n";
      }
    }
  }			     ## end of foreach my $event ( @events ) {
  
  print "\n";
}

sub loadFiles($) {
  ##print "In loadFiles @_\n";
  find(\&isWantedFile, @_ );
}
  
# following gets called recursively for each file in $dir, check $_ to see if you want the file!
sub isWantedFile() {
  push @clusterFiles, $File::Find::name if(/clust\..*\.txt$/); # modify the regex as per your needs or pass it as another arg
}

sub contains {
  my ($file, $search) = @_;

  open my $fh, '<', $file or die $!;

  while (<$fh>) {
    return 1 if /$search/;
  }

  return 0;
}

sub hasClusters($) {
  #my $hasClusters=1;
  my $clusterFile=shift;
  my $hasClusters=contains ("$clusterFile", "NO CLUSTERS FOUND");

  #if ( fgrep { /.*NO CLUSTERS FOUND.*/ } "$clusterFile" ) { 
  #  $hasClusters=0; 
  #}
  return ! $hasClusters;
}

# sub hasClusters($) {
#   my $hasClusters=1;
#   my $clusterFile=shift;
#   if ( fgrep { /.*NO CLUSTERS FOUND.*/ } "$clusterFile" ) { 
#     $hasClusters=0; 
#   }
#   return $hasClusters;
# }

sub annotateAnova() {
  my $clusterList = shift;

  my $where = `env AFNI_WHEREAMI_NO_WARN=y whereami -space $space -coord_file $clusterList'[$columns]' -tab `;
  open(WHERE_MEMORY, '<', \$where)
    or die "Can't open memory file: $!\n";

  # skip the first three lines
  <WHERE_MEMORY>;
  <WHERE_MEMORY>;
  <WHERE_MEMORY>;

  my $ORS = $/;
  my $NRS = "+++++++ nearby Atlas structures +++++++\n";

  $/ = $NRS;

  my $recordCount=0;

  my @TailarachLocations = ();

  while (<WHERE_MEMORY>) {
    ++$recordCount;
    chomp;

    $/ = $ORS;

    my $record = $_;
    #print $_;

    open(RECORD, '<', \$record)
      or die "Can't open record file: $!\n";

    while (<RECORD>) {
      ##if (/(?:CA_N27_ML)|(?:TT_Daemon)\s*([^\s]*)\s*([ \w-]*)\s*.*/) {
      if (/(?:TT|CA)(?:[\w]*)\s*([^\s]*)\s*([ \d\w\(\)-]*)\s*.*/) {
	my $TTLocation = $2;
	my $location = $2;

	$location =~ s/Left/L/;
	$location =~ s/Right/R/;
	$location =~ s/Gyrus/Gy/;

	# $location =~ s/Anterior Cingulate/ACC/;
	# $location =~ s/Cingulate/Cing/;

	$location =~ s/Superior/Sup/;
	$location =~ s/Inferior/Inf/;

	# $location =~ s/Middle/Mid/;
	# $location =~ s/Frontal/Fron/;
	# $location =~ s/Anterior/Ant/;
	# $location =~ s/Temporal/Temp/;
	# $location =~ s/Parietal/Par/;
	# $location =~ s/Posterior/Post/;
	# $location =~ s/Medial/Med/;
	# $location =~ s/Cerebellar/Cere/;
	# $location =~ s/Tonsil/Ton/;
	# $location =~ s/Lentiform/Lent/;
	# $location =~ s/Nucleus/Nuc/;
	# $location =~ s/Lobule/Lbl/;
	# $location =~ s/Thalmus/Thal/;
	# $location =~ s/Hippocampus/Hipp/;
	# $location =~ s/Parahippocampal/PHipp/;
	# $location =~ s/Brodmann area/BA/;
	# $location =~ s/Precentral/Prec/;
	# $location =~ s/Postcentral/Postc/;
	# $location =~ s/Caudate/Caud/;
	# $location =~ s/Precuneus/Prec/;
	# $location =~ s/Occipital/Occ/;
	# $location =~ s/callosal/call/;
	# $location =~ s/Supramarginal/Smar/;

	push @TailarachLocations, trim(trimWhiteSpaces($location));
	#print "$TTLocation : $location\n";
	last;
      } elsif (/\*\*\*\*\* Not near any region stored in databases \*\*\*\*\*/) {
	my $location = "No Region";
	push @TailarachLocations, trim(trimWhiteSpaces($location));
	last;
      }
    }

    $/ = $NRS;
  }

  $/ = "\n";

  return (@TailarachLocations);
}				# end of sub annotateAnova

sub trimWhiteSpaces($)
  {
    my $string = shift;
    $string =~ s/\s+/ /g;

    return $string;
  }

# Perl trim function to remove whitespace from the start and end of the string
sub trim($)
  {
    my $string = shift;
    $string =~ s/^\s+//;
    $string =~ s/\s+$//;
    return $string;
  }

# Left trim function to remove leading whitespace
sub ltrim($)
  {
    my $string = shift;
    $string =~ s/^\s+//;
    return $string;
  }

# Right trim function to remove trailing whitespace
sub rtrim($)
  {
    my $string = shift;
    $string =~ s/\s+$//;
    return $string;
  }
