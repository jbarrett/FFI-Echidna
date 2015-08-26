use strict;
use warnings;
use 5.020;
use experimental qw( signatures postderef );

package FFI::Echidna {

  # ABSTRACT: Developer tools for FFI
  
  package FFI::Echidna::Type {
  
    use Moose::Util::TypeConstraints;
    use MooseX::Getopt ();
    use namespace::autoclean;

    subtype 'FFI::Echidna::Type::RegexpRef'
    => as 'RegexpRef';
    
    subtype 'FFI::Echidna::Type::DirList'
    => as 'ArrayRef[Path::Class::Dir]';

    subtype 'FFI::Echidna::Type::FileList'
    => as 'ArrayRef[Path::Class::File]';
    
    coerce 'FFI::Echidna::Type::RegexpRef'
    => from 'Str'
    => via { qr{$_} };
    
    coerce 'FFI::Echidna::Type::DirList'
    => from 'ArrayRef[Str]'
    => via { [map { Path::Class::Dir->new($_) } $_->@*] };

    coerce 'FFI::Echidna::Type::FileList'
    => from 'ArrayRef[Str]'
    => via { [map { Path::Class::File->new($_) } $_->@*] };
    
    MooseX::Getopt::OptionTypeMap->add_option_type_to_map(
      'FFI::Echidna::Type::RegexpRef' => '=s',
    );
    
  }

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

    sub _framework_path ($self, $path) {
      if($^O eq 'darwin' && $path =~ s/ \(framework directory\)//) {
        $path = Path::Class::Dir->new($path);
        my $fake_dir = FFI::Echidna::FS->homedir->subdir('.echidna', 'frameworks', join('.', grep !/^$/, $path->dir_list), 'include');
        $fake_dir->mkpath(0, 0700);
        unlink $_ for $fake_dir->children;
        foreach my $old (map { $_->subdir('Headers') } grep { $_->is_dir && $_->basename =~ /\.framework$/ } $path->children) {
          my $new = $fake_dir->file($old->parent->basename =~ s/\.framework$//r);
          use autodie qw( symlink );
          symlink $old, $new;
        }
        return $fake_dir;
      } else {
        Path::Class::Dir->new($path);
      }
    }
    
    # ccflags = -x c++ for C++
    sub include_paths ($self) {
      my $empty = FFI::Echidna::FS->tempfile("standardXXXX", SUFFIX => '.h');
      my $result = $self->run($self->cpp_flags->@*, '-E', $empty, '-v')->die_on_error;
      my @paths = split /\n\r?/, $result->stderr;
      shift @paths while @paths && $paths[0] !~ /^#include \<\.\.\.\> search starts here:/;
      $result->die("unable to parse header files") unless @paths;
      shift @paths;
      pop @paths;
      [map { $self->_framework_path($_) } map { s/^\s+//r }@paths];
    }

    sub cpp ($self, $path) {
      $self->run($self->cpp_flags->@*, '-E', $path)->die_on_error->stdout;
    }

    sub _standard_headers_example ($class) {
      state $empty;
      unless(defined $empty) {
        $empty = FFI::Echidna::FS->tempfile("standardXXXX", SUFFIX => '.h');
        $empty->spew("#include <stdio.h>\n#include <stddef.h>\n#include <stdint.h>\n#include <inttypes.h>\n");
      }
      $empty;
    }    
    
    sub all_macros ($self, $path=undef) {
      my %macros;
      
      unless(defined $path) {
        $path = __PACKAGE__->_standard_headers_example;
      }
      
      foreach my $line (split /\n\r?/, $self->run($self->cpp_flags->@*, qw( -dM -E ), $path)->die_on_error->stdout) {
        if($line =~ /^#define ([A-Za-z_][A-Za-z_0-9]*) (.*)$/) {
          $macros{$1} = $2;
        }
      }
      
      \%macros;
      
    }
    
    sub base_macros ($self) {
      state $empty;
      unless(defined $empty) {
        $empty = FFI::Echidna::FS->tempfile("standardXXXX", SUFFIX => '.h');
        $empty->spew("\n");
      }
      $self->all_macros($empty);
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
      
      push @out, '';
      
      foreach my $line (@out) {
        $line =~ s/^(?<prefix>[| ]*(\||`)-)// || $line eq '' || die "unable to parse: $line";
        my $prefix = $+{prefix};
        my $count  = length $prefix;
        $current_indent = $count unless defined $current_indent;
        
        if($current_indent < $count) {
          push @stack, [ $current_list, $current_indent ];
          $current_list = $current_list->[-1];
        } else {
          if(ref $current_list->[-1] eq 'ARRAY' && $current_list->[-1]->$#* == 0) {
            $current_list->[-1] = ($current_list->[-1]->[0]);
          }
          if($current_indent > $count) {
            pop @stack while $stack[-1]->[1] > $count;
            $current_list = (pop @stack)->[0];
          }
        }
        $current_indent = $count;
        last if $line eq '';
        
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
  
    # This started out life as a model based on the output of
    # clang (hence the name), but now it is a model based on
    # the output of clang and optionally the output of
    # Convert::Binary::C (cbc).
  
    use FFI::Echidna::OO qw( MooseX::Types::Path::Class );

    around BUILDARGS => sub ($orig, $class, $path, %attr) {

      my $clang = $attr{clang} = FFI::Echidna::ClangWrapper->new($attr{clang}||={});
      
      unless(-r $path) {
        my $tmp_path = FFI::Echidna::FS->tempfile("clang_model_XXXXXX", SUFFIX => ".h");
        $tmp_path->spew("#include <$path>\n");
        $path = $tmp_path;
      }
      
      $attr{header} = $path;
    
      $class->$orig(\%attr);

    };
    
    has header => (
      is       => 'ro',
      isa      => 'Path::Class::File',
      coerce   => 1,
      required => 1,
    );

    has ast => (
      is       => 'ro',
      isa      => 'FFI::Echidna::ClangAstNode',
      lazy     => 1,
      default  => sub ($self) {
        $self->clang->ast($self->header);
      },
    );
    
    has macros => (
      is       => 'ro',
      lazy     => 1,
      default  => sub ($self) {
        $self->clang->macros($self->header);
      },
    );
    
    has cbc => (
      is      => 'ro',
      isa     => 'Convert::Binary::C',
      lazy    => 1,
      default => sub ($self) {
        my @text = split /\n\r?/, $self->clang->cpp($self->header);

        my $text = join "\n", 
          map {
            s/^# ([0-9]+) "(.*?)".*$/#line $1 "$2"/r
          } @text;
        
        require Convert::Binary::C;
        my $cbc = Convert::Binary::C->new;
        
        $cbc->parse(join "\n",
          "typedef void * __builtin_va_list;",
          "#define __attribute__(arg)",
          "#define __asm__(arg)",
          "#define __restrict",
          "#define __extension__",
        );
        
        $cbc;
      },
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
            $model->constant_class->new( name => $name, value => $macros->{$name} ),
          );
        }
        
        $ast = $self->ast;
      }
    
      if($ast->type eq 'TypedefDecl') {
      
        if($ast->data =~ /^(?<alias>.*?)\s+'(?<type>.*?)(':'(?<real_type>.*?))?'$/) {
          push @items, $model->filter_typedefs(
            $model->typedef_class->new(\%+)
          );
        } else {
          warn "unable to parse TypedefDecl: " . $ast->data;
        }
      
      } elsif($ast->type eq 'FunctionDecl') {
      
        my $param_counter = 1;
      
        if($ast->data =~ /^(implicit\s+)?(?<name>[A-Za-z_][A-Za-z_0-9]+) '(const )?(?<return_type>.*)\(/) {
          my $name = $+{name};
          my $return_type = $+{return_type};
          my @args = map {
            my $ast = $_;
            $ast->data =~ /^(const\s+)?((?<name>.*?) )?'(?<type>[^']+)(':'(?<real_type>[^']+))?'$/
            ? do {
                my %args = %+;
                $args{name} ||= 'param' . $param_counter++;
                FFI::Echidna::ModuleModel::Function::Argument->new(%args);
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
            $model->function_class->new(
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
      $node =~ s{\<(\<invalid sloc\>|[^>]+)\>\s+(([^ ]+:[0-9]+(:[0-9]+|)?\s+|\<invalid sloc\>\s*))?}{};
      $node =~ s{prev 0x[0-9a-f]+\s*}{};

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
    
    sub to_string ($self) {
      join ':', $self->filename, $self->line, $self->column;
    }

  }

  package FFI::Echidna::ModuleModel {
  
    use FFI::Echidna::OO;

    foreach my $name (qw( Constant Typedef Function )) {
      has lc($name).'s' => (
        is      => 'ro',
        isa     => "ArrayRef[FFI::Echidna::ModuleModel::$name]",
        default => sub { [] },
        lazy    => 1,
      );
      constant->import(lc($name).'_class' => "FFI::Echidna::ModuleModel::$name");
      no strict 'refs';
      *{'filter_'.lc($name).'s'} = sub { $_[1] };
    }
    
    has _hash => (
      is      => 'ro',
      default => sub { {} },
      lazy    => 1,
    );
    
    sub lookup_constant ($self, $name) {
      $self->_hash->{constants}->{$name};
    }

    sub lookup_typedef ($self, $alias) {
      $self->_hash->{typedefs}->{$alias};
    }
    
    sub lookup_function ($self, $name) {
      $self->_hash->{functions}->{$name};
    }

    sub add ($self, @items) {
      foreach my $item (@items) {
      
        if($item->isa('FFI::Echidna::ModuleModel::Constant')) {
          push $self->constants->@*, $item;
          $self->_hash->{constants}->{$item->name} = $item;
        }
        
        elsif($item->isa('FFI::Echidna::ModuleModel::Typedef')) {
          push $self->typedefs->@*, $item;
          $self->_hash->{typedefs}->{$item->alias} = $item;
        }
        
        elsif($item->isa('FFI::Echidna::ModuleModel::Function')) {
          push $self->functions->@*, $item;
          $self->_hash->{functions}->{$item->name} = $item;
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
  
  package FFI::Echidna::Template {
    
    use Moose::Role;
    
    has include_path => (
      is      => 'ro',
      lazy    => 1,
      default => sub {
        [FFI::Echidna::FS->sharedir->subdir('tt')],
      },
    );
    
    requires 'process';
    
  };
  
  package FFI::Echidna::Template::TT {
  
    use Data::Dumper qw( Dumper );
    use FFI::Echidna::OO;
    
    with 'FFI::Echidna::Template';

    has tt => (
      is      => 'ro',
      lazy    => 1,
      default => sub ($self) {
        require Template;
        require Template::Context;
        my $c = Template::Context->new({
          INCLUDE_PATH => [ map { $_->stringify } $self->include_path->@* ],
          FILTERS      => {
            perl_constant_value => sub ($value) {
              do { no warnings; eval $value };
              # TODO: use Data::Dumper to dump this to a valid
              # Perl string, accounting for quotes and such.
              local $Data::Dumper::Terse = 1;
              $@ ? Dumper($value) =~ s/\s*$//r : $value;
            },
          },
        });
        
        $c->define_vmethod(
          hash => perl_render => sub ($node) {
            if(eval { $node->isa('FFI::Echidna::ModuleModel::Typedef') }) {
              return sprintf "'%s' => '%s'", $node->type, $node->alias;
            } else {
              die "perl_render does not know how to handle a ", ref $node;
            }
          },
        );
        
        Template->new({ CONTEXT => $c });
      },
    );
    
    sub process ($self, $input, $vars, $output=undef) {
    
      $input = "$input" if eval { $input->isa('Path::Class::File') };
      $output = "$output" if defined $output && eval { $output->isa('Path::Class::File') };
      $self->tt->process($input, $vars, $output) || die $self->tt->error;
      
      return;
    }
    
    BEGIN { $INC{'FFI/Echidna/Template/TT.pm'} = __FILE__ }
  
  }
  
}

1;

__END__

=head1 SYNOPSIS

 % h2ffi \
   --perl_package_name OpenGL::FFI::Mesa::GL \
   --libname GL \
   --filter_constant ^GL_ \
   --filter_typedef ^GL \
   --filter_function ^gl \
   GL/gl.h

=head1 DESCRIPTION

The goal of L<FFI::Echidna> is to provide tools for writing FFI
modules in Perl using L<FFI::Platypus> (and in the future possibly
other FFI tool sets and languages).  I expect the first tangible
and public interface to be L<h2ffi>, which will generate a Perl
extension to an existing library using its C header files in the
same vein as L<h2xs>.  In the longer term I'd like to add tools
to interface with other languages (both on the DLL side and on the
scripting/VM side).

I intend on using the L<FFI::Echidna> API in order to build
L<OpenGL::FFI>, which will support a number of different OpenGL
implementations, each with subtle incompatibilities, and hundreds
of functions and constants.  At the moment, you should consider
the API to be quite unstable (not to mention undocumented).  If
that doesn't deter you, you should feel free to try this out.  A
good place to start, is perhaps L<OpenGL::FFI> itself which will
serve as a working example.

=head1 CAVEATS

Requires clang.  Works fine with a Perl built with gcc, but you will need
clang in your path.

=head1 SEE ALSO

=over 4

=item L<h2ffi>

=item L<FFI::Platypus>

=back

=cut
