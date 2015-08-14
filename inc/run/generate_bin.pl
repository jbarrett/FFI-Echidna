use strict;
use warnings;
use 5.020;
use Path::Class qw( file dir );
use Template;
use lib 'lib';

my $template = do { local $/; <DATA> };
my $tt = Template->new;

foreach my $pm (grep { !$_->is_dir } dir(qw( lib App ))->children)
{
  my $app = $pm->basename;
  $app =~ s/\.pm$//;
  my @data = $pm->slurp;
  shift @data while $data[0] !~ /^# ABSTRACT: (.*)$/;
  die "unable to find abstract in $app" unless @data;
  my $abstract = $1;
  shift @data;
  my $bin = file('bin', $app);
  
  say $bin;
  $tt->process(\$template, { app => $app, abstract => $abstract, pod => join('', @data) }, $bin->stringify);
  
  chmod 0755, $bin;
}

__DATA__
#!/usr/bin/perl

# NOTE: this file is generated.  Documentation should be updated in
# lib/App/[% app %].pm instead and then re-run inc/run/generate_bin.pl

use strict;
use warnings;
use 5.020;
use App::[% app %];

# PODNAME: [% app %]
# ABSTRACT: [% abstract %]

exit App::[% app %]->main(@ARGV);

__END__

[% pod %]
