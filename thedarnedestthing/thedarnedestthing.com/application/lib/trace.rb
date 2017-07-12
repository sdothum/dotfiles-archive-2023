# encoding: UTF-8

# the darnedest thing
# ▬▬▬▬▬▬▬▬▬▬▬▬▬▬▬▬▬▬▬▬▬▬▬▬▬▬▬▬▬▬▬▬▬▬▬▬▬▬▬▬▬▬▬▬▬▬▬▬▬▬▬▬▬▬▬▬▬▬▬▬▬▬▬▬▬▬▬▬▬▬▬▬▬▬▬▬▬▬

# ...................................................... Debug logging functions

require 'term/ansicolor'

class String
    include Term::ANSIColor
end

class Trace
  attr_accessor   :threshold    # :trace => -1 to suppress all messages

  def initialize(level = 0, logfile =  nil)
    @log = logfile ? File.new(logfile, 'a') : $stdout
    @log.sync = true
    @threshold = level
    @threshold ||= 0
    info(":trace level => #{@threshold}") unless @threshold < 0
  end

  def trace(level, message)
    if level <= @threshold or @verbose
      @log.write Time.now.strftime('%-I:%M:%S%P ').green.bold
      @log.write "/#{level}/ ".red.bold if level > 0
      @log.write "#{message}\n"
    end
  end

  def debug!
    @debug = @debug ? nil : true
    CurrentState.call(@debug)
  end

  def verbose!
    @verbose = @verbose ? nil : true
    CurrentState.call(@verbose)
  end

  def info(message)
    trace(0, message.cyan.bold)
  end

  def debug(message)
    trace(0, message) if @debug or @verbose
  end

  # def verbose(message)
  #   trace(0, message) if @verbose
  # end

  def warning(message)
    trace(0, "<WARNING> #{message}".yellow.bold)
  end

  def error(message)
    trace(0, "<ERROR> #{message}".red.bold)
  end
end

