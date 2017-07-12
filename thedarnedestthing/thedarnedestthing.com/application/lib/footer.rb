# encoding: UTF-8

# the darnedest thing
# ▬▬▬▬▬▬▬▬▬▬▬▬▬▬▬▬▬▬▬▬▬▬▬▬▬▬▬▬▬▬▬▬▬▬▬▬▬▬▬▬▬▬▬▬▬▬▬▬▬▬▬▬▬▬▬▬▬▬▬▬▬▬▬▬▬▬▬▬▬▬▬▬▬▬▬▬▬▬

# .................................................................. Page footer

class Footer
  def initialize(cssclass = 'history')
    @links = ''
    @separator = ''
    @cssclass = cssclass
  end

  def << href
    # escape href ?var= assignments, otherwise ? is a regex operator
    unless @links =~ /#{href.gsub(/\?/, '\?')}/
      @links << %[#{@separator}<span class="nobr">#{href}</span>]
      @separator = " #{SEPARATOR} "
    end
  end

  def links
    @links > '' ? %[<p id="footer" class="#{@cssclass}"><br>#{@links}</p>] : ''
  end
end
