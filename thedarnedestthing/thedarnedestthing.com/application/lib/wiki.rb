# encoding: UTF-8

# the darnedest thing
# ▬▬▬▬▬▬▬▬▬▬▬▬▬▬▬▬▬▬▬▬▬▬▬▬▬▬▬▬▬▬▬▬▬▬▬▬▬▬▬▬▬▬▬▬▬▬▬▬▬▬▬▬▬▬▬▬▬▬▬▬▬▬▬▬▬▬▬▬▬▬▬▬▬▬▬▬▬▬

# ....................................................... Wiki article functions

class Wiki
  attr_reader     :content
  attr_reader     :draft
  attr_reader     :filename
  attr_reader     :homepage
  attr_reader     :href
  attr_reader     :lock
  attr_reader     :publishing
  attr_reader     :show_comments
  attr_reader     :thread
  attr_reader     :title
  attr_reader     :uri

  def initialize(uri, *options)
    LOG.debug("Wiki => #{uri}")
    @uri = uri.sub(/&.*/, '')
    @title = File.title(@uri)
    @secret = uri.sub(/.*&/, '').to_i == @title.to_i(36)
    link_next = options.find_index(:link_next)
    @show_comments = options.find_index(:show_comments)
    if thread_exist?
      @publishing = DayMonthYear.call(File.date(@filename))
      @content = WikiContent.call(read_vimwiki(@filename))
    else
      @content = WikiContent.call(not_found)
    end
    @href = %[<a href="/#{@uri}">#{TOC.call([@title, @draft, @lock, @homepage])}</a>]
    unless (HideArticle.call(@homepage) and not @secret)
      unless @secret
        insert_link(:next) { WikiList.new(@thread) } if link_next
        insert_link(:comment) if options.find_index(:comment)
        if @show_comments
          insert_comments
          insert_link(:comment)
        end
      end
      xref_wikis("#{XREF}/#{@title}") if link_next and Dir.exist?("#{XREF}/#{@title}")
    end
    LOG.debug("uri => #{@uri} title => #{@title} thread => #{@thread} filename => #{@filename}")
  end

  def profile
    [ @uri, @title, @draft, @lock, @homepage ]
  end

  def read_vimwiki(filename)
    @content = File.zero?(filename) ? not_found : markup_extensions(Kramdown::Document.new(markdown_vimwiki(IO.read(filename))).to_html)
    @draft = @content =~ /\[draft\]/
    @lock = @content =~ /\[lock\]/
    @content = @content.gsub(/\[lock\] */, '')
    @content = not_found if (HideArticle.call(@homepage, (@draft or @lock)) and not @secret)
    @content
  end

  # def markdown_vimwiki(document)
  #   document
  #     .gsub(/\[\[([^|\[\]]*)\]\]/, '[\1](http://#{SERVER}/\1)')
  #     .gsub(/\[\[(https*:\/\/[^\]]*)[|]([^\[]*)\]\]/, '[\2](\1)')
  #     .gsub(/\[\[\?\|([^\[]*)\]\]/, '[\1](http://#{SERVER}/search?query=\1)')
  #     .gsub(/\[\[([^\]]*)\|([^\[]*)\]\]/, '[\2](http://#{SERVER}/\1)')
  #     .gsub(/!\[([^\]]*)\]\(([^")]*)\)/, '![\1](\2 "\1")')
  #     .gsub(/\^\/(.*)\^/, '<a href="http://#{SERVER}/\1">\1</a>')
  #     .gsub(/\^\?(.*)\^/, '<span class="quicksearch"><a href="http://#{SERVER}/search?query=\1">\1</a></span>')
  #     .gsub(/#\{SERVER\}/, "#{SERVER}#{PORT}")
  #     .gsub(/^(=+ .*) =+/, '\1')
  #     .gsub(/\[ljust\]/, '<span class="ljust"></span>')
  #     .gsub(/ *\[tag\](.*)\n/, '<span class="tag"></span>')
  #     .gsub(/^======/, '######')
  #     .gsub(/^=====/, '#####')
  #     .gsub(/^====/, '####')
  #     .gsub(/^===/, '###')
  #     .gsub(/^==/, '##')
  #     .gsub(/^=/, '#')
  #     .gsub(/^\*\*\*\*\*\* (.*)/, '###### \1 ######')
  #     .gsub(/^\*\*\*\*\* (.*)/, '##### \1 #####')
  #     .gsub(/^\*\*\*\* (.*)/, '#### \1 ####')
  #     .gsub(/^\*\*\* (.*)/, '### \1 ###')
  #     .gsub(/^\*\* (.*)/, '## \1 ##')
  #     .gsub(/^\* (.*)/, '# \1 #')
  #     .gsub(/{{{.*/, '')
  #     .gsub(/}}}/, '')
  #     .gsub(/__([a-zA-Z0-9])/, '**\1')
  #     .gsub(/([a-zA-Z0-9])__/, '\1**')
  #     .gsub(/''([a-zA-Z0-9])/, '*\1')
  #     .gsub(/([a-zA-Z0-9])''/, '\1*')
  #     .gsub(/_\\_/, '__')
  #     .gsub(/^%template .*/, '')
  # end

  def markdown_vimwiki(document)
    document
      .gsub(/\[([^\[\]]*)\]\[\]/, '[\1](http://#{SERVER}/\1)')
      .gsub(/\[\[([^|\[\]]*)\]\]/, '[\1](http://#{SERVER}/\1)')
      .gsub(/\[\[(https*:\/\/[^\]]*)[|]([^\[]*)\]\]/, '[\2](\1)')
      .gsub(/\[([^\]]*)\]\[\?\]/, '[\1](http://#{SERVER}/search?query=\1)')
      .gsub(/\[\[\?\|([^\]]*)\]\]/, '[\1](http://#{SERVER}/search?query=\1)')
      .gsub(/\[([^\]]*)\]\[([^\]]*)\]/, '[\1](http://#{SERVER}/\2)')
      .gsub(/\[\[([^\]]*)\|([^\[]*)\]\]/, '[\2](http://#{SERVER}/\1)')
      .gsub(/!\[([^\]]*)\]\(([^")]*)\)/, '![\1](\2 "\1")')
      .gsub(/\^\/(.*)\^/, '<a href="http://#{SERVER}/\1">\1</a>')
      .gsub(/\^\?(.*)\^/, '<span class="quicksearch"><a href="http://#{SERVER}/search?query=\1">\1</a></span>')
      .gsub(/#\{SERVER\}/, "#{SERVER}#{PORT}")
      .gsub(/^(=+ .*) =+/, '\1')
      .gsub(/\[ljust\]/, '<span class="ljust"></span>')
      .gsub(/ *\[tag\](.*)\n/, '<span class="tag"></span>')
      .gsub(/^======/, '######')
      .gsub(/^=====/, '#####')
      .gsub(/^====/, '####')
      .gsub(/^===/, '###')
      .gsub(/^==/, '##')
      .gsub(/^=/, '#')
      .gsub(/{{{.*/, '')
      .gsub(/}}}/, '')
      .gsub(/^%template .*/, '')
  end

  def markup_extensions(document)
    # insert </p> to close any paragraph containing an image to preserve
    # the slim javascript "resize" margin positioning!
    # note caret juggling!
    document
      .gsub(/\^ /, '&nbsp;')
      .gsub(/\^'/, '`')
      .gsub(/\^~/, '`')
      .gsub(/\^\^/, '<caret>')
      .gsub(/\[\[\]/, '[[')
      .gsub(/\[\]\]/, ']]')
      .gsub(/\^\|(.*)/, '<p class="center">\1</p>')
      .gsub(/\^\$/, '<br>')
      .gsub(/<caret>/, '^')
      .gsub(/<img src=([^<]*\/(.*).(jpg|png)[^>]*)>/, %q[</p><a class="image" href=\1><img id="\2" src=\1></a>])
  end

  def plain_text(word_count = 9000)
      Sanitize.clean(@content.gsub(/(&nbsp;)+/, ' ')).split(/ +/)[0..word_count].join(' ')
  end

  def words(word_count = 9000)
    Sanitize.clean(@content.gsub(/<span class="tag">(.*)<\/span>/, '\1').gsub(/\n/, ' ').gsub(/(&nbsp;)+/, ' ')).split(/ +/)[0..word_count].uniq.join(' ').downcase
  end

  def insert_link(link)
    case link
    when :about
      @content << %[<p class="nextwiki"><a href="/about #{@title}">#{ARROW}about</a></p>]
    when :start_here
      @content << %[<p class="nextwiki"><a href="/welcome">#{ARROW}start here</a></p>]
      @content << '<p class="showcomments"><a name="recent comments" href="/recent comments">recent comments</a></p>'
    when :gallery
      @content << '<p class="showcomments"><a name="gallery" href="/gallery">gallery</a></p>'
    when :next
      next_uri = yield.find_next_uri(@uri)
      @content << %[<p class="nextwiki"><a href="/#{next_uri}">#{ARROW}#{File.title(next_uri)}</a></p>] if next_uri
      insert_link(:comment) if @uri == @title or IsDate.call(@uri) or next_uri =~ /gallery/
      insert_link(:secret) if @thread == 'notes' or @lock
    when :comment
      @content << %[<p class="showcomments">]
      if comments?
        @content <<
          if @show_comments
            %[<a href="/#{@uri}">hide</a>&nbsp;&nbsp;&nbsp;#{SEPARATOR}&nbsp;&nbsp;&nbsp;]
          else
            %[#{comments_href("show")}&nbsp;&nbsp;&nbsp;#{SEPARATOR}&nbsp;&nbsp;&nbsp;]
          end
      end
      @content << %[<a href="mailto:comments@thedarnedestthing.com?subject=/// #{@uri} ///">comment</a><a href="/comments"> ?</a></p>]
    when :secret
      @content << %[<p class="secret"><a href="#{TDT_URI}/#{@uri}&#{@title.to_i(36)}">unlock</a></p>]
    end
  end

  def comments?
    Dir.glob("#{WIKI}/mail/comments/#{@uri}/*").count > 0
  end

  def insert_comments
    LOG.info('Building "comment" article references')
    @content << '<a name="comments"><hr></a>'
    Dir.glob("#{WIKI}/mail/comments/#{@uri}/*").each { |comment| @content << IO.read(comment) }
  end

  def thread_exist?
    @filename = nil
    @thread = nil
    if @uri =~ /\//
      @thread = @uri.sub(/(.*)\/.*/, '\1')
      @filename = "#{WIKI}/#{@thread}/diary/#{@title}.#{VIMWIKI_EXT}"
      @homepage = THREADS[@thread]
      @thread = "#{@thread}/diary"
      LOG.debug("thread => #{@thread} filename => #{@filename} homepage => #{@homepage}")
    else
      THREADS.each do |thread, homepage|
        if File.exist?("#{WIKI}/#{thread}/#{@title}.#{VIMWIKI_EXT}")
          @filename = "#{WIKI}/#{thread}/#{@title}.#{VIMWIKI_EXT}"
          @thread = thread
          @homepage = homepage
          break
        end
      end
    end
    @filename
  end

  def not_found
    @is_empty = true
    '<p>this page has not yet been updated. Please check back later..</p>'
  end

  def empty?
    @is_empty
  end

  def comments_href(description = '<span class="comments">[comments]</span>')
    %[<a href="/#{comments_uri}">#{description}</a>]
  end

  def comments_uri
    "#{@uri}?show#comments"
  end

  def encode_uri
    @uri.gsub(/ /, '%20')
  end

  def xref_wikis(directory)
    titles = Dir.glob("#{directory}/*")
    xref = Footer.new('xref')
    titles.sort.each do |filename|
      title = File.title(filename)
      xref << %[<a href="/#{title =~ /\d\d\d\d-\d\d-\d\d/ ? "journal/#{title}" : "#{title}"}">#{title}</a>]
    end
    @content << xref.links
  end

  def body
    body = %[<h1 class="title"><a href="/#{@uri}">#{IsDate.call(@title) ? @publishing[:date] : @title}</a></h1>]
    body << %[<p class="publishing"><a href="/date?period=#{@publishing[:year]}##{@publishing[:month]}">#{@publishing[:date]}</a></p>]
    insert_link(:comment)
    body << @content
    body
  end
end

# ................................................................ Comment entry

class Comment < Wiki
  def initialize(filename)
    LOG.info("Comment => #{filename.sub(/.*\/comments\//, '')}")
    @content = IO.read(filename).sub(/<p class="date">.*<\/p>\n/, '')
    @title = File.title(filename.sub(/\/#{File.basename(filename)}/, ''))
    @uri = filename =~ /\/gallery\// ? filename.sub(/.*(gallery\/[^\/]*).*/, '\1') : filename.sub(/.*\/comments\/(.*)\/[^\/]*/, '\1')
    @href = comments_href(@title)
    LOG.debug("title => #{@title} uri => #{uri} href => #{@href}")
    thread_exist?
  end
end

# .......................................................... Thread introduction

class Intro < Wiki
  attr_reader     :thread

  def initialize(title)
    LOG.info("Intro => #{title}")
    @title = title
    @thread = THREADS.key(title)
    @content = WikiContent.call(introduction)
    insert_link(title == HOMEPAGE ? :start_here : :about)
    insert_link(:gallery) if title == 'shadows and light'
  end

  def introduction
    content = nil
    Dir.glob("#{WIKI}/**/#{title}.#{VIMWIKI_EXT}").each do |filename|
      unless File.zero?(filename)
        content = read_vimwiki(filename)
        break
      end
    end
    content ? content : not_found
  end
end

# ................................................... Thread diary journal entry

class Diary < Wiki
  attr_reader     :thread
  attr_reader     :history

  def initialize(title)
    LOG.info("Diary => #{title}")
    @title = title
    @history = Footer.new('history')
    @thread = THREADS.key(title)
    @content = WikiContent.call(last_diary_entry)
  end

  def last_diary_entry
    content = nil
    Dir.glob("#{WIKI}/#{@thread}/diary/2*.#{VIMWIKI_EXT}").sort.reverse.each do |filename|
      unless File.zero?(filename)
        unless content
          uri = File.uri(filename)
          wiki = Wiki.new(uri, :comment)
          content = %[<h1 class="diary"><a href="/#{uri}">#{DayMonthYear.call(File.date(filename))[:date]}</a></h1>]
          content << wiki.content
        else
          month = DayMonthYear.call(File.date(filename))[:month]
          @history << %[<a href="/#{@thread}?period=#{month}">#{month}</a>]
        end
      end
    end
    content ? content : ''
  end
end

# ................................................................ Image gallery

class Gallery < Wiki
  def initialize(uri, *options)
    LOG.debug("Gallery => #{uri}")
    @uri = uri
    @title = File.basename(uri)
    @show_comments = options.find_index(:show_comments)
    @content = '<div class="grid"><div class="gallery"><ul class="rig columns-3">'
    Dir.glob("#{IMAGES}/**/#{@title}/*_1.jpg").sort.each do |filename|
      thumbnail = ImageUri.call(filename)
      href = thumbnail.gsub(/_1/, '')
      gallery = href
        .gsub(/\/[^\/]*$/, '')
        .gsub(/.*\/images(\/\d*\.\w*){0,1}/, '')
        .gsub(/\/(\d*\.)*/, "#{BULLET}")
        .gsub(/^#{BULLET}/, '') unless gallery
      filename = href.gsub(/.*\//, '')
      camera =
        filename =~ /SDIM/ ? 'sigma dp1' :
        filename =~ /DSCF/ ? 'fuji x100' :
        nil
      if filename =~ /^\d{8}/
        ymd = filename.gsub(/^(\d{8}).*/, '\1')
        date = "#{FormatDate.call(Date.parse(ymd))}"
        camera =
          ymd < '20080415' ? 'olympus e-1' :
          ymd < '20090507' ? 'sigma dp1' :
          ymd < '20090513' ? 'olympus e-1' :
          ymd < '20110604' ? 'sigma dp1' :
          ymd < '20141114' ? 'fuji x100' :
          'fuji x100t' unless camera
      else
        date = %[#{File.date("#{PUBLIC}#{href}")}]
      end
      camera = "#{CIRCLE}#{camera}" if camera
      @content << %[<li><a href="#{href}" title="#{gallery}<br><span class=exif>#{date}#{camera}</span>"><img src="#{thumbnail}"></a></li>]
    end
    @content << '</ul></div></div>'
    insert_link(:next) { GalleryList.new }
    if @show_comments
      insert_comments
      insert_link(:comment)
    end
    LOG.debug("content => #{@content}")
  end
end
