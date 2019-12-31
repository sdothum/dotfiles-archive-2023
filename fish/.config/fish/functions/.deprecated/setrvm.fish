function setrvm
  function version
    if [ -d ~/.rvm/rubies/ruby-$argv ]
      set -x -g GEM_HOME ~/.rvm/gems/ruby-$argv
      set -x -g GEM_PATH ~/.rvm/gems/ruby-$argv ~/.rvm/gems/ruby-$argv@global
      set -x -g IRBRC ~/.rvm/rubies/ruby-$argv/.irbrc
      set -x -g MY_RUBY_HOME ~/.rvm/rubies/ruby-$argv
      set -x -g PATH ~/.rvm/gems/ruby-$argv/bin ~/.rvm/gems/ruby-$argv@global/bin ~/.rvm/rubies/ruby-$argv/bin ~/.rvm/bin $PATH
      set -x -g RUBY_VERSION ruby-$argv
      set -x -g rvm_env_string ruby-$argv
      set -x -g rvm_ruby_string ruby-$argv
      set -x -g rvm_bin_path ~/.rvm/bin
      set -x -g rvm_path ~/.rvm
      set -x -g rvm_prefix ~
      set -x -g rvm_version "1.15.8 (stable)"
    else
      echo "rvm ruby version $argv not installed"
    end
  end

  switch "$argv[1]"
    case [1];     setrvm 2.0
    case clear;   set -e GEM_HOME
                  set -e GEM_PATH
                  set -e IRBRC
                  set -e MY_RUBY_HOME
                  set -e PATH
                  set -e RUBY_VERSION
                  set -e rvm_env_string
                  set -e rvm_ruby_string
                  set -e rvm_bin_path
                  set -e rvm_path
                  set -e rvm_prefix
                  set -e rvm_version
                  set -e JRUBY_OPTS
    case env;     env | a 'ruby|rvm'
    case 1.8;     version 1.8.7-p370
                  set -x -g JRUBY_OPTS --1.8
    case 1.9;     version 1.9.3-p286
                  set -x -g JRUBY_OPTS --1.9
    case 2.0;     version 2.0.0-p0
                  set -x -g JRUBY_OPTS --2.0
    case '*';     echo ".. setrvm  clear | env | 1.8 | 1.9 | 2.0"
  end
end
