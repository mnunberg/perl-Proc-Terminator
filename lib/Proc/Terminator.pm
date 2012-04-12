package Proc::Terminator::Ctx;
use strict;
use warnings;
use POSIX qw(errno_h);
use Class::Struct;

my $DEBUG = $ENV{PROC_TERMINATOR_DEBUG};

struct 'Proc::Terminator::Ctx' =>
    [
     'pid' => '$',
     'siglist' => '*@',
     'last_sent' => '$'
    ];


sub try_kill {
    my ($self,$do_kill) = @_;
    
    if (kill(0, $self->pid) == 0) {
        my $errno_save = $!;
        $DEBUG and warn "Kill with signal=0 returned 0 (dead!)";
        if ($errno_save != ESRCH) {
            warn $errno_save;
            return -1;
        }
        # else, == ESRCH
        return 1;
    }
    
    if (!$do_kill) {
        $DEBUG and warn "We were not requested to proceed with signal. Returning";
        return 0;
    }
    my $sig = shift @{$self->siglist};

    if (!defined $sig) {
        $DEBUG and warn "Cannot kill ${\$self->pid} because no signals remain";
        return -1;
    }
    $DEBUG and warn "Using signal $sig for ${\$self->pid}";
    
    if (kill($sig, $self->pid) == 1) {
        return 0;
    }
    
    if ($! == ESRCH) {
        return 1;
    } else {
        warn $!;
        return -1;
    }
}

package Proc::Terminator;
use warnings;
use strict;
use Time::HiRes qw(time sleep);
use POSIX qw(:signal_h :sys_wait_h :errno_h);
use base qw(Exporter);

our $VERSION = 0.02;

our @DefaultSignalOrder = (
    SIGINT,
    SIGQUIT,
    SIGTERM,
    SIGKILL
);

our @EXPORT = qw(proc_terminate);

# Kill a bunch of processes
sub proc_terminate {
    my ($pids, %options) = @_;
    my $siglist = delete $options{siglist} || [ @DefaultSignalOrder ];
    my %procs;
    $pids = ref $pids ? [ @$pids ] : [$pids];
    foreach my $pid (@$pids) {
        $procs{$pid} = Proc::Terminator::Ctx->new(
            pid => $pid,
            siglist => [ @$siglist ],
            last_sent => 0);
    }
    my $max_wait = delete $options{max_wait} || 10; # 10 seconds
    my $sleep_interval = delete $options{interval} || 0.25; #msecs
    my $grace_period = delete $options{grace_period} || 0.75;
    
    my $begin_time = time();
    my $now;
    
    my @badprocs;
    
    while (%procs && ($now = time()) - $begin_time < $max_wait) {
        while (my ($pid,$ctx) = each %procs) {
            
            my $do_send_kill = $now - $ctx->last_sent > $grace_period;
            
            if ($do_send_kill) {
                $ctx->last_sent($now);
            }
            
            $DEBUG and warn("Trying to kill? $do_send_kill");
            my $ret = $ctx->try_kill($do_send_kill);
            
            if ($ret) {
                # non-zero return code, either error or successful termination.
                # in any event, we can't do any more.
                delete $procs{$pid};
                if ($ret == -1) {
                    push @badprocs, $pid;
                }
            }
        }
        $DEBUG and warn("Sleeping for $sleep_interval msecs");
        sleep($sleep_interval);
    }
    
    while (my ($pid,$whatever) = each %procs) {
        if (kill(0, $pid) == 0 && $! == ESRCH) {
            delete $procs{$pid};
        }
    }
    
    if (%procs) {
        warn("Processes still remain");
        return undef;
    }
    
    if (wantarray) {
        return @badprocs;
    } else {
        return !@badprocs;
    }
}

__END__

=head1 NAME

Proc::Terminator - Conveniently terminate processes

=head1 SYNOPSIS

    use Proc::Terminator;
    
    # Try and kill $pid using various methods, waiting
    # up to 20 seconds
    
    proc_terminate($pid, max_wait => 20);

=head1 DESCRIPTION

C<Proc::Terminator> provides a convenient way to kill a process, often useful in
utility and startup functions which need to ensure the death of an external
process.

This module provides a simple, blocking, and procedural interface to kill
a process or multiple processes (not tested), and not return until they are
all dead.

C<Proc::Terminator> can know if you do not have permissions to kill a process,
if the process is dead, and other interesting tidbits.

It also provides for flexible options in the type of death a process will
experience. Whether it be slow or immediate.

This module exports a single function, C<proc_terminate>

=head2 C<proc_terminate($pids, %options)>

Will try to terminate C<$pid>, waiting until the process is no longer alive, or
until a fatal error happens (such as a permissions issue).

C<$pid> can either be a single PID (a scalar), or a reference to an array of
I<multiple> PIDs, in which case they are all attempted to be killed, and the
function only returning once all of them are dead (or when no possible kill
alternatives remain).

The C<%options> is a hash of options which control the behavior for trying to
terminate the pid(s).

=over

=item C<max_wait>

Specify the time (in seconds) that the function should try to spend killing the
provided PIDs. The function is guaranteed to not wait longer than C<max_wait>.

This parameter can also be a fractional value (and is passed to L<Time::HiRes>).

I<DEFAULT>: 10 Seconds.

=item C<siglist>

An array of signal constants (use L<POSIX>'s C<:signal_h> to get them).

The signals are tried in order, until there are no more signals remaining.

Sometimes applications do proper cleanup on exit with a 'proper' signal such as
C<SIGINT>.

The default value for this parameter

The default signal list can be found in C<@Proc::Terminator::DefaultSignalOrder>

I<DEFAULT>: C<[SIGINT, SIGQUIT, SIGTERM, SIGKILL]>

=item C<grace_period>

This specifies a time, in seconds, between the shifting of each signal in the
C<siglist> parameter above.

In other words, C<proc_terminate> will wait C<$grace_period> seconds after sending
each signal in C<siglist>. Thereafter the signal is removed, and the next signal
is attempted.

Currently, if you wish to have controlled signal wait times, you can simply
insert a signal more than once into C<siglist>

I<DEFAULT>: 0.75

=item C<interval>

This is the loop interval. The loop will sleep for ever C<interval> seconds.
You probably shouldn't need to modify this

I<DEFAULT>: 0.25

=back

When called in a scalar context, returns true on sucess, and false otherwise.

When called in list context, returns a list of the PIDS B<NOT> killed.

=head1 SEE ALSO

L<signal(7)>

L<kill(2)>

L<Perl's kill | kill>

=head1 AUTHOR & COPYRIGHT

Copyright (C) 2012 M. Nunberg

You may use and distribute this software under the same terms and conditions
as Perl itself.
