#!/usr/bin/perl -w
# hihoo - extract Hoogle .HOO information from GHC .hi interface files
# by Gaal Yahas <gaal@forum2>

use strict;
use Getopt::Long;
use File::Find;
use Text::Balanced;

our $VERSION = '0.07';

# ChangeLog
#
# 0.08
# - most of the required support for classes, only one subtest fails
#   (emits `instance Class2 (Data2 a)` instead of
#   `instance Eq a => Class2 (Data2 a)`) - how bad do we want this?
#
# 0.07
# - pass all tests except the ones for classes
# - parse data completely
# - supress duplicate accessors from different variants
# - newtype (processed just like data declarations, I hope that's good)
#
# 0.06
# - dequalify all outputs, not just from function signatures.
# - use a record-based iterator instead of parsing lines, bozhe moi!
# - support operators
# - improve data type parsing, including infix constructors and field accessors
#
# 0.05
# - fix a bug in multi-line processing that caused some functions
#   to be omitted
# - handle instance declarations
#
# 0.04
# - strip function dependencies from class signatures
# - give up more gracefully on funky functions (?ref / {1})


# Functions matching these patterns are considered to be internal GHC stuff.
# "Of course, this is a heuristic, which is a fancy way of saying that
# it doesn't work." -- MJD
our @STOPWORDS = map { qr/$_/ } qw/^a[\d\s] ^lvl[\d\s] ^lvn[\d\s] ^\$/;

# GHC types which don't need to be fully qualified.
our @DEQUALIFY = map { qr/\b$_\./ } map { quotemeta }
        qw/GHC.Base GHC.Conc GHC.IOBase GHC.Num GHC.Prim GHC.Read GHC.Show/;

GetOptions \our %Config, qw(ghc|g=s);
$Config{ghc} ||= 'ghc';

for my $e (@ARGV) {
    if (-d $e) { recurse($e) } else { do_process($e) }
}
exit 0;

sub recurse {
    find({ wanted => \&process, no_chdir => 1}, shift);
    exit 0;
}

sub process {
    do_process($File::Find::name); # ick globals
}

sub do_process {
    my($file) = @_;
    return unless $file =~ /\.hi$/;

    my $info = mk_iface_stream($file);

    print "-- $file\n";
    my %operators;
    while (defined($_ = $info->())) {
    #print "[$_]\n";
        /^((\S+)( :: [^{\n]+))/  && do { my $f = $operators{$2} ? "($2)" : $2;
                                         printfunc("$f$3") unless stop($2) };
        /^interface (\w+)/       && do { print "module $1\n" };
        # why do i sometimes see "2 class" in the hi output?
        /^\d* \s* (class.*)/sx   && do { process_class($1) };
        /^type/                  && do { s/\s+/ /g; s/\s*Variances.*//; dprint($_) };
        /^(instance .*) =/       && do { dprint($1) };
        /^data|newtype/          && do { process_data($_) };
        /^export Operators (.*)/ && do { load_operators($1, \%operators) };
    }

    print "\n";
}

sub load_operators {
    my($ops, $store) = @_;
    # --show-iface is redundant: infix data constructors are marked as
    # such elsewhere, but also appear here. So let's strip 'em and handle
    # them later, in context. This is a little hacky.
    $ops =~ s/\S+\{.*?\}//g;
    %$store = map { $_ => 1 } split /\s+/, $ops;
}

sub process_class {
    my($class) = @_;
    my($classname) = $class =~ /class \s* (.*?) \s* Var/sx or do {
        warn "*** class declaration unknown:\n$class"; return };
    dprint("class $classname");
    $classname =~ s/.* => \s*//; # crude, but correct AFAIK
    $class =~ s/\{-.*?-\}//g; # remove "{- has default method -}"
    while ($class =~ s/(\S+) \s* :: \s* (.+?)\s*(?=(?:\S+\s*:)|$)//x) {
        dprint("$1 :: $classname => $2");
    }
}

sub process_data {
    my($data) = @_;
    my %seenfields;
    (my($decl, $type, $variants) = $data =~ m{
        ^(data|newtype) \s* (.*?) \s*
        Variances .*?
        = \s*
        (.*) \s*             # constructors, with Stricts and Fields etc.
    }sx) or do { warn "*** can't parse data or newtype declaration:\n$data"; return };
    dprint("$decl $type");
    for my $v (split /\s+\|\s+/, $variants) {
        my($cons, $params, $infix, $stricts, $fields) = $v =~ m{
            ^
            (\S+)                       \s*  # Cons
            (.*?)                       \s*  #       Int String (IO a)
            (Infix)?                    \s*  #
            (?:Stricts: \s* ([\s_!]*))? \s*  # !Int
            (?:Fields:  \s* (.*))? \s*       # { x :: Num, y :: Num }
            $
        }x or do { warn "*** can't parse data declaration:\n$data"; return };
        my @params  = psplit($params);
        my @stricts = map { /^!/ ? '!' : "" } split /\s+/, ($stricts||'');
        my @fields  = split /\s+/, ($fields||'');

#::YY([$data, $cons, \@params, \@stricts, \@fields]) if $type eq 'Data1 a';
        my $cons1 = $infix ? "($cons)" : $cons;
        dprint("$cons1 :: ". join " -> ", @params, $type);
        for (0 .. $#fields) {
            dprint("$fields[$_] :: $type -> $params[$_]") unless
                $seenfields{$fields[$_]}++;
        }
    }
}

sub psplit {
    map { /^\(/ ? $_ : split }
        Text::Balanced::extract_multiple(shift,
            [ sub { Text::Balanced::extract_bracketed($_[0],"()") } ] );
}

sub mk_iface_stream {
    my($file) = @_;
    open my $fh, "$Config{ghc} --show-iface $file |" or die "open: $file: $!";
    my $buf;
    return sub {
        return undef unless $fh;
        #die "stream exhausted" unless $fh;
        while (<$fh>) {
            if (/^\S/ && $buf) {
                my $tmp = $buf;      # "return $buf; $buf = $_" :-)
                $buf = $_;
                return $tmp;
            }
            $buf .= $_;
        }
        close $fh or die "close: $file: $!";
        undef $fh;
        return $buf;
    }
}


sub dprint {
    my(@strs) = @_;
    for my $str (@strs) {
        $str =~ y/\n//d;
        $str =~ s/$_//g for @DEQUALIFY;
    }
    $strs[-1] =~ s/\s+$//;
    print @strs, "\n";
    #print ">>> ", @strs, "\n";
}

sub printfunc {
    my($str) = @_;

    if ($str =~ /\?ref|\{/) {
        warn "*** Function too funky for us, please add this definition yourself:\n$str";
        return;
    }
    
    dprint($str);
}

sub stop {
    my($word) = @_;
    do { return 1 if $word =~ $_ } for @STOPWORDS;
}
sub ::Y { require YAML; YAML::Dump(@_) }
sub ::YY { require Carp; Carp::confess(::Y(@_)) }

