package Releaser;

use v5.30.1;
use strict;
use warnings;
use feature 'postderef', 'signatures';
use warnings 'FATAL' => 'all';
use autodie qw( :all );
use namespace::autoclean;

use DateTime::Format::Human::Duration;
use DateTime::Format::RFC3339;
use DateTime;
use File::pushd qw( pushd );
use FindBin qw( $Bin );
use Git::Helpers qw( current_branch_name );
use Git::Sub qw( commit diff push tag );
use HTTP::Request;
use JSON::MaybeXS qw( decode_json encode_json );
use LWP::UserAgent;
use MIME::Base64 qw( encode_base64 );
use Path::Tiny qw( path );
use Specio::Library::Builtins;
use Specio::Library::String;
use URI::FromHash qw( uri );
use YAML::XS qw( Dump Load );

use Moose;
## no critic (TestingAndDebugging::ProhibitNoWarnings)
no warnings 'experimental::postderef', 'experimental::signatures';
## use critic

with 'MooseX::Getopt::Dashes';

has _azure_authz_header => (
    is       => 'ro',
    isa      => t( 'ArrayRef', of => t('NonEmptyStr') ),
    init_arg => undef,
    lazy     => 1,
    default  => sub ($self) {
        return [
            Authorization => 'Basic '
                . encode_base64(
                join q{:}, $self->_azure_username, $self->_azure_token
                ),
        ];
    },
);

has _azure_username => (
    is       => 'ro',
    isa      => t('NonEmptyStr'),
    init_arg => undef,
    lazy     => 1,
    default  => 'houseabsolute',
);

has _azure_token => (
    is       => 'ro',
    isa      => t('NonEmptyStr'),
    init_arg => undef,
    lazy     => 1,
    default  => path( $ENV{HOME}, '.azure-release-token' )->slurp
);

has _branch => (
    is       => 'ro',
    isa      => t('NonEmptyStr'),
    init_arg => undef,
    lazy     => 1,
    default  => current_branch_name(),
);

has _github_authz_header => (
    is       => 'ro',
    isa      => t( 'ArrayRef', of => t('NonEmptyStr') ),
    init_arg => undef,
    lazy     => 1,
    default  => sub ($self) {
        return [ Authorization => 'token ' . $self->_github_token ];
    },
);

has _github_token => (
    is       => 'ro',
    isa      => t('NonEmptyStr'),
    init_arg => undef,
    lazy     => 1,
    default  => path( $ENV{HOME}, '.github-token' )->slurp
);

has _ua => (
    is       => 'ro',
    isa      => 'LWP::UserAgent',
    init_arg => undef,
    lazy     => 1,
    default  => sub { LWP::UserAgent->new },
);

sub run ($self) {
    die 'This can only be run from the current branch'
        unless $self->_branch eq 'master';

    my ( $tag, $notes ) = $self->_check_and_update_changes;
    $self->_commit_tag_and_push($tag);
    $self->_wait_for_ci_perl_helpers_build;
    my %queued = $self->_update_integration_projects;
    $self->_wait_for_builds_to_finish(
        DateTime::Duration->new( hours => 1 ),
        'integration test builds',
        %queued,
    );
    $self->_update_github_release( $tag, $notes );

    return 0;
}

sub _check_and_update_changes {
    my $changes = path( $Bin, '..', '..', 'Changes.md' )->realpath;
    my $content = $changes->slurp_utf8;

    my $today = DateTime->today( time_zone => 'local' )->ymd;
    $content
        =~ s/\A(\#\# +(\d+\.\d+\.\d+))(\n\n)(\*.+?)(?=\#)/$1 $today$3$4/msa
        or die "Could not find a pending release in $changes\n";
    my $tag   = $2;
    my $notes = $4;

    $changes->spew_utf8($content);

    return 'v' . $tag, $notes;
}

sub _commit_tag_and_push ( $self, $tag ) {
    git::commit( '-a', '-m', "Add release date for $tag to Changes.md" );
    git::tag($tag);
    git::push('--tags');
    git::push();
}

# This is the ID of the pipeline for this project.
my $CIPerlHelpersID
    = git::remote( 'show', 'origin' ) =~ /\Qgit\@github.com:autarch/ ? 20 : 7;

sub _wait_for_ci_perl_helpers_build ($self) {
    my $cutoff = DateTime->now->subtract( minutes => 5 );

    my $desc = 'ci-perl-helpers build';
    my ($build) = $self->_wait_for_builds_to_queue(
        $desc,
        [$CIPerlHelpersID],
        $cutoff,
    );

    $self->_wait_for_builds_to_finish(
        DateTime::Duration->new( hours => 2 ),
        $desc,
        'autarch/ci-perl-helpers' => {
            build_id => $build->{build}{id},
            at       => $self->_dt( $build->{queueTime} ),
        },
    );

    return;
}

# The id numbers can be seen when looking at the pipeline in the browser.
my %IntegrationProjects = (
    'houseabsolute/ciph-integration-eumm'    => 16,
    'houseabsolute/ciph-integration-mb'      => 17,
    'houseabsolute/ciph-integration-minilla' => 18,
    'houseabsolute/ciph-integration-dzil'    => 19,
);

sub _update_integration_projects ($self) {
    my $want_ref
        = $self->_branch eq 'master' ? undef : 'refs/heads/' . $self->_branch;

    my %pushed;
    for my $project ( sort keys %IntegrationProjects ) {
        my $short          = ( split /\//, $project )[1];
        my $dir            = path( $ENV{HOME}, 'projects', $short );
        my $pipelines_yaml = $dir->child('azure-pipelines.yml');
        my $azure          = LoadFile($pipelines_yaml);

        next
            if $want_ref
            && $azure->{resources}{repositories}[0]{ref}
            && $want_ref eq $azure->{resources}{repositories}[0]{ref};
        next
            if !$want_ref
            && !$azure->{resources}{repositories}[0]{ref};

        if ($want_ref) {
            $azure->{resources}{repositories}[0]{ref} = $want_ref;
        }
        else {
            delete $azure->{resources}{repositories}[0]{ref};
        }
        DumpFile( $pipelines_yaml, $azure );

        next unless git::diff();

        my $pushed = pushd($dir);
        git::commit(
            '-a',
            '-m',
            sprintf( 'Switch ci-perl-helpers to %s branch', $self->_branch ),
        );
        git::push();
        $pushed{ $IntegrationProjects{$project} } = $project;
    }

    return unless keys %pushed;

    return $self->_queued_builds_for(%pushed);
}

sub _queued_builds_for ( $self, %pushed ) {
    my $cutoff = DateTime->now->subtract( minutes => 5 );

    my $desc = join q{, }, sort keys %pushed;
    $desc .= ' builds';

    # We're just waiting until all the builds are queued, which should happen
    # quickly, as opposed to waiting for them to finish.
    my $sleep_time = 5;
    my $waited     = 0;

    my %queued;
    for my $matched (
        $self->_wait_for_builds_to_queue( $desc, [ keys %pushed ], $cutoff ) )
    {
        my $id = $matched->{build}{definition}{id};
        $queued{ $pushed{$id} } = {
            build_id => $matched->{build}{id},
            at       => $matched->{at},
        };
    }

    return %queued;
}

sub _wait_for_builds_to_queue ( $self, $desc, $definition_ids, $cutoff ) {
    my $sleep_time      = 5;
    my $wait            = 120;
    my $started_waiting = DateTime->now;
    my $formatter       = DateTime::Format::Human::Duration->new;

    # We're just waiting until the build is queued, which should happen
    # quickly, as opposed to waiting for it to finish.
    while (1) {
        my $now = DateTime->now( time_zone => 'local' );
        say sprintf(
            'Checking for queued %s - waited for %s',
            $desc,
            $formatter->format_duration_between(
                $started_waiting,
                $now,
                units => [qw( hours minutes seconds )],
            ),
        ) or die $!;

        my @builds = $self->_find_matching_builds( $definition_ids, $cutoff );
        if (@builds) {
            say sprintf(
                '  found %d queued builds that matched our criteria',
                scalar @builds
            ) or die $!;
        }
        else {
            say '  did not find any queued builds that matched our criteria'
                or die $!;
        }
        return @builds if scalar @builds == scalar $definition_ids->@*;

        last if $started_waiting->clone->add( seconds => $wait ) < $now;

        sleep $sleep_time;
    }

    my $err = sprintf( 'Waited two minutes for %s to be queued', $desc );
    die $err;
}

sub _wait_for_builds_to_finish ( $self, $wait, $desc, %queued ) {
    my $sleep_time      = 60;
    my $started_waiting = DateTime->now;
    my $formatter       = DateTime::Format::Human::Duration->new;

WAIT:
    while (1) {
        my $now = DateTime->now( time_zone => 'local' );
        for my $project ( sort keys %queued ) {
            say sprintf(
                'Checking on queued build for %s (%d) - running for %s',
                $project,
                $queued{$project}{build_id},
                $formatter->format_duration_between(
                    $queued{$project}{at},
                    $now,
                    units => [qw( hours minutes seconds )],
                ),
            ) or die $!;
            if (
                $self->_build_is_done(
                    $project, $queued{$project}{build_id}
                )
            ) {
                say "  $project is done";
                delete $queued{$project};
                last WAIT unless keys %queued;
            }
        }

        if ( $started_waiting->clone->add( seconds => $wait ) < $now ) {
            my $err = sprintf(
                'Waited %s for all %s to complete but these did not - %s',
                $formatter->format_duration($wait),
                $desc,
                join q{ }, sort keys %queued,
            );
            die $err;
        }

        sleep $sleep_time;
    }

    return;
}

sub _queue_build ( $self, $project, $definition_id ) {
    my $uri      = $self->_make_azure_uri('/build/builds');
    my $response = $self->_request(
        'POST',
        $uri,
        $self->_azure_authz_header,
        {
            definition  => { id => $definition_id },
            triggerInfo => 'Pre-release integration testing',
        },
    );
    say "Queued build $response->{id} for $project"
        or die $!;

    return ( $response->{id}, $self->_dt( $response->{queueTime} ) );
}

sub _find_matching_builds ( $self, $definition_ids, $cutoff ) {
    my $uri = $self->_make_azure_uri(
        '/build/builds',
        maxBuildsPerDefinition => 1,
        definitions            => ( join ',', $definition_ids->@* ),
        statusFilter           => 'inProgress,notStarted,postponed',
        minTime => DateTime::Format::RFC3339->format_datetime($cutoff),
    );

    my $builds = $self->_request(
        'GET',
        $uri,
        $self->_azure_authz_header,
    );

    my @matched;
    for my $build ( $builds->{value}->@* ) {
        my $qt = $self->_dt( $build->{queueTime} );
        push @matched, { build => $build, at => $qt };
    }

    return @matched;
}

sub _build_is_done ( $self, $project, $build_id ) {
    my $build = $self->_request(
        'GET',
        $self->_make_azure_uri("/build/builds/$build_id"),
        $self->_azure_authz_header,
    );
    if ( $build->{status} eq 'completed' ) {
        if ( $build->{result} eq 'succeeded' ) {
            say "  completed successfully"
                or die $!;
            return 1;
        }
        die "Build for $project completed with result = $build->{result}\n";
    }
    elsif ( $build->{status} eq 'cancelling' ) {
        die "Build for $project was cancelled\n";
    }

    say "  status = $build->{status}"
        or die $1;

    return 0;
}

sub _update_github_release ( $self, $tag, $notes ) {
    $notes =~ s/\A\s+|\s+\z//g;
    $self->_request(
        'POST',
        'https://api.github.com/repos/autarch/ci-perl-helpers/releases',
        $self->_github_authz_header,
        {
            tag_name => $tag,
            name     => $tag,
            body     => $notes,
        },
    );

    return;
}

sub _make_azure_uri ( $self, $path, %query ) {
    return uri(
        scheme => 'https',
        host   => 'dev.azure.com',
        path   => '/houseabsolute/houseabsolute/_apis' . $path,
        query  => {
            %query,
            'api-version' => '5.1',
        },
        query_separator => '&',
    );

    return;
}

sub _dt ( $self, $raw ) {
    return DateTime::Format::RFC3339->parse_datetime($raw)
        ->set_time_zone('local');
}

sub _request ( $self, $method, $uri, $authz = [], $body = undef ) {
    my @headers = (
        $authz->@*,
        'Content-Type' => 'application/json',
    );

    my $content;
    if ($body) {
        $content = encode_json($body);
    }

    my $req = HTTP::Request->new( $method, $uri, \@headers, $content );

    my $resp = $self->_ua->request($req);
    if ( $resp->is_success ) {
        my $content = $resp->decoded_content;
        return unless length $content;
        return decode_json($content);
    }

    die $resp->as_string;
}

__PACKAGE__->meta->make_immutable;

1;
