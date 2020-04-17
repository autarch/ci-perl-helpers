use v5.30.1;
use strict;
use warnings;
use autodie qw( :all );

use FindBin qw( $Bin );
use lib "$Bin/../lib";

use Releaser;

exit Releaser->new_with_options->run;
