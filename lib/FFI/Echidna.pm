use strict;
use warnings;
use 5.020;
use experimental qw( signatures postderef );

package FFI::Echidna {

  # ABSTRACT: Developer tools for FFI

  package FFI::Echidna::OO {
  
    use Import::Into;

    # in principle, I agree with the idea of Moose turning on
    # warnings.  In principal I agree with warning about
    # experimental features.  I disagree with turning on
    # warnings that I explicitly have turned off.  Since I've
    # decided to use experimental features (postderef
    # is not experimental anymore in 5.24, and I find it
    # unlikely that signatures will removed) we define our
    # own strictures and warnings and override Moose's
    # (usually reasonable) preferences.
    
    # also, add namespace::autoclean.  Wow there is a lot
    # of boiler plate in order to do stuff in moose.  Note
    # that this means that the use FFI::Echidna::OO should
    # be the LAST use statement before the actual guts of
    # the class.
    
    sub import ($class, @modules) {
      no warnings 'uninitialized';
      my $old = ${^WARNING_BITS};
      unshift @modules, 'Moose';
      push @modules, 'namespace::autoclean';
      while(@modules) {
        my $module = shift @modules;
        my $pm = "$module.pm";
        $pm =~ s{::}{/}g;
        require $pm;
        my @args = ref $modules[0] eq 'ARRAY' ? (shift @modules)->@* : ();
        my $caller = caller;
        $module->import::into($caller, @args);
      }
      ${^WARNING_BITS} = $old;
      return;
    }
    
    BEGIN { $INC{"FFI/Echidna/OO.pm"} = __FILE__ }
  
  }

  package FFI::Echidna::ProcessCaptureResult {

    use Capture::Tiny qw( capture );
    use Carp qw( croak );
    use FFI::Echidna::OO;

    around BUILDARGS => sub ($orig, $class, @command_line) {
      
      my($stdout, $stderr, $errno, $child_error) = capture {
        local $!;
        system @command_line;
        ($!,$?);
      };
      
      $class->$orig({
        command_line => \@command_line,
        stdout       => $stdout,
        stderr       => $stderr,
        errno        => $errno,
        child_error  => $child_error,
      });

    };

    has $_ => ( is => 'ro', required => 1 ) for qw( command_line stdout stderr errno child_error );

    sub command ($self) {
      $self->command_line->[0];
    }
    
    sub signal ($self) {
      $self->child_error & 127;
    }
    
    sub exit ($self) {
      $self->child_error >> 8;
    }
    
    sub error ($self) {
      if($self->child_error == -1) {
        return $self->command . " failed to execute: " . $self->errno;
      } elsif($self->signal) {
        return $self->command . " died with signal " . $self->signal;
      } elsif($self->exit) {
        return $self->command . " exited with value " . $self->exit;
      } else {
        return '';
      }
    }
    
    sub die_on_error ($self) {
      my $error = $self->error;
      $self->die($error) if $error;
      $self;
    }
    
    sub die ($self, $error) {
      say STDERR "% @{$self->command_line}";
      say STDERR "[out]\n@{[ $self->stdout ]}" if $self->stdout ne '';
      say STDERR "[err]\n@{[ $self->stderr ]}" if $self->stderr ne '';
      croak $error;
    }
    
    __PACKAGE__->meta->make_immutable;
  }

  package FFI::Echidna::FS {

    use File::Temp ();
    use File::ShareDir qw( dist_dir );
    use File::HomeDir ();
    use FFI::Echidna::OO qw( MooseX::Singleton MooseX::Types::Path::Class );

    has tempdir => (
      is      => 'ro',
      isa     => 'Path::Class::Dir',
      coerce  => 1,
      default => sub { File::Temp::tempdir( CLEANUP => 1 ) },
      lazy    => 1,
    );
    
    sub tempfile ($class, @rest) {
      my($fh, $filename) = $class->tempdir->tempfile(@rest);
      close $fh;
      Path::Class::File->new($filename);
    }
    
    has sharedir => (
      is      => 'ro',
      isa     => 'Path::Class::Dir',
      coerce  => 1,
      lazy    => 1,
      default => sub {
        if($ENV{FFI_ECHIDNA_DIR} && -d $ENV{FFI_ECHIDNA_DIR}) {
          return $ENV{FFI_ECHIDNA_DIR};
        }
        if(defined $FFI::Echidna::FS::VERSION) {
          my $dir = eval { dist_dir('FFI-Echidna') };
          return $dir unless $@;
        }
        my $dir = Path::Class::File->new($INC{'FFI/Echidna.pm'})
          ->absolute
          ->parent
          ->parent
          ->parent;
        $dir = $dir->parent if $dir->basename eq 'blib';
        $dir = $dir->subdir(qw( share ));
        return $dir if -d $dir;
        die "unable to find share dir";
      },
    );
    
    has homedir => (
      is      => 'ro',
      isa     => 'Path::Class::Dir',
      coerce  => 1,
      lazy    => 1,
      default => sub ($self) {
        my $dir = eval { File::HomeDir->my_home };
        return $dir if defined $dir && -d $dir;
        $dir = $self->tempdir->subdir('fakehome');
        $dir->mkpath(0, 0700);
        $dir;
      },
    );
    
    __PACKAGE__->meta->make_immutable;
  }

  package FFI::Echidna::ClangWrapper {
  
    use File::Which qw( which );
    use FFI::Echidna::OO qw( MooseX::Types::Path::Class );
    
    has clang_path => (
      is      => 'ro',
      isa     => 'Path::Class::File',
      coerce  => 1,
      lazy    => 1,
      default => sub {
        my $path = which('clang');
        die "unable to find clang" unless defined $path;
        $path;
      },
    );
    
    has cpp_flags => (
      is      => 'ro',
      default => sub { [] },
      lazy    => 1,
    );

    has version => (
      is      => 'ro',
      lazy    => 1,
      default => sub ($self) {
        my $result = $self->run("--version")->die_on_error;
        my $out = $result->stdout;
        if($out =~ /^clang version (?<version>[0-9.]+)/) {
          return $+{version};
        } elsif($out =~ /based on LLVM (?<version>[0-9.]+(svn)?)/) {
          return $+{version};
        } else {
          $result->die("unable to determine version");
        }
      },
    );
    
    sub run ($self, @command_line) {
      FFI::Echidna::ProcessCaptureResult->new(
        $self->clang_path, 
        '-fno-color-diagnostics',
        @command_line,
      );
    }

    sub all_macros ($self, $path=undef) {
      my %macros;
      
      unless(defined $path) {
        state $empty;
        unless(defined $empty) {
          $empty = FFI::Echidna::FS->tempfile("standardXXXX", SUFFIX => '.h');
          $empty->spew("#include <stdio.h>\n#include <stddef.h>\n#include <stdint.h>\n#include <inttypes.h>\n");
        }
        $path = $empty;
      }
      
      foreach my $line (split /\n\r?/, $self->run($self->cpp_flags->@*, qw( -dM -E ), $path)->die_on_error->stdout) {
        if($line =~ /^#define ([A-Za-z_][A-Za-z_0-9]*) (.*)$/) {
          $macros{$1} = $2;
        }
      }
      
      \%macros;
      
    }
    
    sub macros ($self, $path) {
      my $mine    = $self->all_macros($path);
      
      state $default;
      $default = $self->all_macros unless defined $default;
      
      delete $mine->{$_} for keys %$default;
      
      foreach my $key (keys %$mine) {
        my $value = $mine->{$key};
        unless($value =~ /^(0x[A-Za-z0-9]+|-?[0-9]+|".*")$/) {
          delete $mine->{$key};
        }
      }
      
      $mine;
    }
    
    sub ast_list ($self, $path) {
      my($first, @out) = split /\n\r?/, $self->run($self->cpp_flags->@*, qw( -Xclang -ast-dump -fsyntax-only ), $path)->die_on_error->stdout;
      
      my @ast = ($first);
      
      my $current_list = \@ast;
      my $current_indent;
      my @stack;
      my $filename  = '';
      my $linenumber = '';
      my $col;
      
      foreach my $line (@out) {
        $line =~ s/^(?<prefix>[| ]*(\||`)-)// || die "unable to parse: $line";
        my $prefix = $+{prefix};
        my $count  = length $prefix;
        $current_indent = $count unless defined $current_indent;
        
        my @locations;
        
        while($line =~ m{(?<id>[^< ]*?):(?<pos1>[0-9]+)(:(?<pos2>[0-9]+))?}g) {
         
          my $id = $+{id};
          
          if($id eq 'col') {
            $col = $+{pos1};
          } else {
            if($id ne 'line') {
              $filename = $id;
            }
            $linenumber = $+{pos1};
            $col = $+{pos2};
          }
          
          my $location = "$filename:$linenumber:$col";
          push @locations, $location;
        }
        
        $line = "$line echidna_location(@locations)";
        
        if($current_indent < $count) {
          push @stack, $current_list;
          $current_list = $current_list->[-1];
        } else {
          if(ref $current_list->[-1] eq 'ARRAY' && $current_list->[-1]->$#* == 0) {
            $current_list->[-1] = ($current_list->[-1]->[0]);
          }
          if($current_indent > $count) {
            $current_list = pop @stack;
          }
        }
        $current_indent = $count;
        
        push @$current_list, [ $line ];
      }
      
      \@ast;
    }
    
    sub ast ($self, $path) {
      FFI::Echidna::ClangAstNode->new($self->ast_list($path));
    }
    
    __PACKAGE__->meta->make_immutable;
  }
  
  package FFI::Echidna::ClangModel {
  
    use FFI::Echidna::OO;

    around BUILDARGS => sub ($orig, $class, $path, %attr) {

      my $clang = $attr{clang} = FFI::Echidna::ClangWrapper->new($attr{clang}||={});
      
      unless(-r $path) {
        my $tmp_path = FFI::Echidna::FS->tempfile("clang_model_XXXXXX", SUFFIX => ".h");
        $tmp_path->spew("#include <$path>\n");
        $path = $tmp_path;
      }
      
      $attr{ast}    = $clang->ast($path);
      $attr{macros} = $clang->macros($path);
    
      $class->$orig(\%attr);

    };

    has ast => (
      is       => 'ro',
      required => 1,
    );
    
    has macros => (
      is       => 'ro',
      required => 1,
    );
    
    has clang => (
      is       => 'ro',
      isa      => 'FFI::Echidna::ClangWrapper',
      required => 1,
    );

    sub append_to_model ($self, $model, $ast=undef) {
    
      my @items;
      
      unless(defined $ast) {

        # handle macros
        my $macros = $self->macros;
        foreach my $name (grep !/^_/, sort keys %$macros) {
          push @items, $model->filter_constants(
            FFI::Echidna::ModuleModel::Constant->new( name => $name, value => $macros->{$name} ),
          );
        }
        
        $ast = $self->ast;
      }
    
      if($ast->type eq 'TypedefDecl') {
      
        if($ast->data =~ /^(?<alias>.*?)\s+'(?<type>.*?)(':'(?<real_type>.*?))?'$/) {
          push @items, $model->filter_typedefs(
            FFI::Echidna::ModuleModel::Typedef->new(\%+)
          );
        } else {
          warn "unable to parse TypedefDecl: " . $ast->data;
        }
      
      } elsif($ast->type eq 'FunctionDecl') {
      
        if($ast->data =~ /^(?<name>[A-Za-z_][A-Za-z_0-9]+) '(const )?(?<return_type>.*)\(/) {
          my $name = $+{name};
          my $return_type = $+{return_type};
          my @args = map {
            my $ast = $_;
            $ast->data =~ /^(const\s+)?(?<name>.*?) '(?<type>[^']+)(':'(?<real_type>[^']+))?'$/
            ? do {
                FFI::Echidna::ModuleModel::Function::Argument->new(%+);
              }
            : do {
                warn "unable to parse ParmVarDecl: " . $ast->data;
                FFI::Echidna::ModuleModel::Function::Argument->new(
                  name => 'unknown',
                  type => 'unknown',
                );
              };
          } grep { $_->type eq 'ParmVarDecl' } $ast->children->@*;
          
          push @items, $model->filter_functions(
            FFI::Echidna::ModuleModel::Function->new(
              name        => $name,
              return_type => $return_type,
              arguments   => \@args,
            )
          );
        } else {
          warn "unable to parse FunctionDecl: " . $ast->data;
        }
      
      }
      
      $self->append_to_model($model, $_) for $ast->children->@*;
      
      $model->add(@items);
      
    }


    __PACKAGE__->meta->make_immutable;  
  }
  
  package FFI::Echidna::ClangAstNode {

    use FFI::Echidna::OO;

    around BUILDARGS => sub ($orig, $class, $node) {

      my @children;
      
      if(ref($node) eq 'ARRAY') {
        ($node, @children) = @$node;
      }
      
      my %attr;

      $attr{children} = [
        map { __PACKAGE__->new($_) } @children
      ];

      my $fulltext = $node;

      if($node =~ s{^([A-Za-z]+) (0x[0-9a-f]+) }{}) {
        $attr{type} = $1;
        $attr{address} = $2;
      } else {
        die "unable to extract type and address from: $fulltext";
      }

      # remove the unparsed location information      
      $node =~ s{\<(\<invalid sloc\>|[^>]+)\>\s+([^ ]+:[0-9]+(:[0-9]+)?\s+)?}{};

      if($node =~ s{\s+echidna_location\((.*?)\)$}{})
      {
        state $files = {};
        $attr{locations} = [
          map {
            my($path, $line, $column) = split /:/;
            
            $path = $files->{$path} //= Path::Class::File->new($path);
            
            FFI::Echidna::SourceLocation->new(
              filename => $path,
              line     => $line,
              column   => $column,
            );
          } split /\s+/, $1
        ];
      }
      
      $attr{data} = $node;
      
      $class->$orig(\%attr);
    };
    

    has type => (
      is       => 'ro',
      required => 1,
    );
    
    has address => (
      is       => 'ro',
      required => 1,
    );
    
    has locations => (
      is      => 'ro',
      default => sub { [] },
      lazy    => 1,
    );
    
    has data => (
      is       => 'ro',
      required => 1,
    );
    
    has children => (
      is       => 'ro',
      required => 1,
    );
    
    __PACKAGE__->meta->make_immutable;
  }
  
  package FFI::Echidna::SourceLocation {

    use FFI::Echidna::OO qw( MooseX::Types::Path::Class );

    has filename => (
      is       => 'ro',
      isa      => 'Path::Class::File',
      required => 1,
      coerce   => 1,
    );
    
    has line => (
      is       => 'ro',
      isa      => 'Int',
      required => 1,
    );
    
    has column => (
      is       => 'ro',
      isa      => 'Int',
      required => 1,
    );

  }

  package FFI::Echidna::ModuleModel {
  
    use FFI::Echidna::OO;

    foreach my $attr (qw( constants typedefs functions )) {
      has $attr => (
        is      => 'ro',
        default => sub { [] },
        lazy    => 1,
      );
    }
    
    has _hash => (
      is      => 'ro',
      default => sub { {} },
      lazy    => 1,
    );
    
    sub filter_constants { ($_[1]) }
    sub filter_typedefs  { ($_[1]) }
    sub filter_functions { ($_[1]) }

    sub add ($self, @items) {
      foreach my $item (@items) {
      
        if($item->isa('FFI::Echidna::ModuleModel::Constant')) {
          push $self->constants->@*, $item;
          $self->_hash->{$item->name} = $item;
        }
        
        elsif($item->isa('FFI::Echidna::ModuleModel::Typedef')) {
          push $self->typedefs->@*, $item;
          $self->_hash->{$item->alias} = $item;
        }
        
        elsif($item->isa('FFI::Echidna::ModuleModel::Function')) {
          push $self->typedefs->@*, $item;
          $self->_hash->{$item->name} = $item;
        }
        
        else {
          warn "tried to add an object of type ", ref($item), " but I don't know what that is";
        }
      }
    }

    __PACKAGE__->meta->make_immutable;
    
    package FFI::Echidna::ModuleModel::Constant {
        
      use FFI::Echidna::OO;
      
      has name => (
        is => 'ro',
        required => 1,
      );

      has value => (
        is => 'ro',
        required => 1,
      );

      __PACKAGE__->meta->make_immutable;
    }

    package FFI::Echidna::ModuleModel::Typedef {
      
      use FFI::Echidna::OO;

      has type => (
        is       => 'ro',
        required => 1,
      );
      
      has alias => (
        is       => 'ro',
        required => 1,
      );
      
      has real_type => (
        is       => 'ro',
        default  => sub ($self) { $self->type },
        lazy     => 1,
      );
      
      sub is_function_pointer ($self) {
        $self->real_type =~ /\(/;
      }
      
      sub is_pointer ($self) {
        $self->real_type =~ /\*/;
      }
      
      sub ffi_platypus_type ($self, $model) {
        state $types;
        
        unless(defined $types) {
          require FFI::Platypus;
          $types->%* = map { $_ => 1 } FFI::Platypus->types;
        }

        return 'opaque' if $self->type eq 'void *' || $self->type =~ /^struct [A-Za-z_][A-Za-z_0-9]* \*$/;
        return $self->type if defined $types->{$self->type};
        return $self->real_type if defined $types->{$self->real_type};
        
        if($self->real_type =~ /^(.*?) \(\*\)\((.*)\)$/)
        {
          my $return_type = $1;
          my @args = map { /\*/ ? 'opaque' : $_  } map { s{^const\s+}{}r } split /\s*,\s*/, $2;
          return "(" . join(',', @args) . ")->$return_type";
        }
        
        return '';
      }

      __PACKAGE__->meta->make_immutable;
    }

    package FFI::Echidna::ModuleModel::Function {
      
      use FFI::Echidna::OO;
      
      has name => (
        is       => 'ro',
        required => 1,
      );
      
      has return_type => (
        is       => 'ro',
        required => 1,
      );
      
      has arguments => (
        is       => 'ro',
        default  => sub { [] },
        lazy     => 1,
      );

      __PACKAGE__->meta->make_immutable;
      
      package FFI::Echidna::ModuleModel::Function::Argument {

        use FFI::Echidna::OO;

        has name => (
          is       => 'ro',
          required => 1,
        );
        
        has type => (
          is       => 'ro',
          required => 1,
        );

        has real_type => (
          is       => 'ro',
          default  => sub ($self) { $self->type },
          lazy     => 1,
        );
      
        __PACKAGE__->meta->make_immutable;
      }
    }
  }
}

1;
